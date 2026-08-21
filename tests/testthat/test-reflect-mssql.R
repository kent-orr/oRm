testthat::source_test_helpers()

# Reflection against a live SQL Server. Skips unless docker, the odbc package,
# and an ODBC driver for SQL Server are all present.

withr::defer({
    cleanup_mssql_test_db()
}, testthat::teardown_env())

# =============================================================================
# reflect_columns.mssql: types, IDENTITY, keys, nullability, defaults, FKs
# =============================================================================

test_that("reflect_columns.mssql captures types, IDENTITY, PK, nullability, and defaults", {
    engine <- mssql_test_engine()
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    con <- engine$get_connection()
    DBI::dbExecute(con, "DROP TABLE IF EXISTS dbo.users")
    DBI::dbExecute(con, paste(
        "CREATE TABLE dbo.users (",
        "id INT IDENTITY(1,1) PRIMARY KEY,",
        "name NVARCHAR(100) NOT NULL,",
        "code VARCHAR(10) NULL,",
        "balance DECIMAL(10,2) NULL,",
        "notes NVARCHAR(MAX) NULL,",
        "active BIT NOT NULL DEFAULT 1)"
    ))

    Users <- engine$reflect("users")
    f <- Users$fields

    # The dialect is detected from the ODBC Driver argument, not passed in.
    expect_equal(engine$dialect, "mssql")

    expect_true(isTRUE(f$id$primary_key))
    expect_match(toupper(f$id$type), "IDENTITY\\(1,1\\)")
    expect_match(tolower(f$id$type), "^int")

    # nvarchar length is reported in bytes and must be halved.
    expect_equal(tolower(f$name$type), "nvarchar(100)")
    expect_false(isTRUE(f$name$nullable))

    expect_equal(tolower(f$code$type), "varchar(10)")
    expect_true(isTRUE(f$code$nullable))

    expect_equal(tolower(f$balance$type), "decimal(10,2)")
    expect_equal(tolower(f$notes$type), "nvarchar(max)")

    expect_false(is.null(f$active$default))
    expect_s3_class(f$active$default, "sql")
})

test_that("reflect_columns.mssql captures foreign keys and their actions", {
    engine <- mssql_test_engine()
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    con <- engine$get_connection()
    DBI::dbExecute(con, "DROP TABLE IF EXISTS dbo.posts")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS dbo.users")
    DBI::dbExecute(con, "CREATE TABLE dbo.users (id INT IDENTITY(1,1) PRIMARY KEY, name NVARCHAR(100))")
    DBI::dbExecute(con, paste(
        "CREATE TABLE dbo.posts (",
        "id INT IDENTITY(1,1) PRIMARY KEY,",
        "user_id INT NOT NULL REFERENCES dbo.users(id) ON DELETE CASCADE,",
        "title NVARCHAR(200))"
    ))

    Posts <- engine$reflect("posts")
    fk <- Posts$fields$user_id

    expect_s3_class(fk, "ForeignKey")
    expect_equal(fk$ref_schema, "dbo")
    expect_equal(fk$ref_table, "users")
    expect_equal(fk$ref_column, "id")
    expect_equal(toupper(fk$on_delete), "CASCADE")
    # NO_ACTION is the implicit default and must not be rendered.
    expect_null(fk$on_update)
    expect_false(isTRUE(fk$nullable))
})

test_that("reflect_columns.mssql captures composite primary keys", {
    engine <- mssql_test_engine()
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    con <- engine$get_connection()
    DBI::dbExecute(con, "DROP TABLE IF EXISTS dbo.memberships")
    DBI::dbExecute(con, paste(
        "CREATE TABLE dbo.memberships (",
        "org_id INT NOT NULL,",
        "user_id INT NOT NULL,",
        "role NVARCHAR(50),",
        "CONSTRAINT pk_memberships PRIMARY KEY (org_id, user_id))"
    ))

    Memberships <- engine$reflect("memberships")
    f <- Memberships$fields

    expect_true(isTRUE(f$org_id$primary_key))
    expect_true(isTRUE(f$user_id$primary_key))
    expect_false(isTRUE(f$role$primary_key))
    expect_equal(oRm:::pk_fields(Memberships), c("org_id", "user_id"))
})

test_that("reflect_columns.mssql captures cross-schema foreign keys", {
    engine <- mssql_test_engine()
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    con <- engine$get_connection()
    DBI::dbExecute(con, "DROP TABLE IF EXISTS dbo.articles")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS other.authors")
    DBI::dbExecute(con, "IF SCHEMA_ID(N'other') IS NULL EXEC('CREATE SCHEMA other')")
    DBI::dbExecute(con, "CREATE TABLE other.authors (id INT IDENTITY(1,1) PRIMARY KEY, name NVARCHAR(100))")
    DBI::dbExecute(con, paste(
        "CREATE TABLE dbo.articles (",
        "id INT IDENTITY(1,1) PRIMARY KEY,",
        "author_id INT REFERENCES other.authors(id))"
    ))

    Articles <- engine$reflect("articles")
    fk <- Articles$fields$author_id

    expect_s3_class(fk, "ForeignKey")
    expect_equal(fk$ref_schema, "other")
    expect_equal(fk$ref_table, "authors")
    expect_equal(fk$references, "other.authors.id")
})

# =============================================================================
# reflect_tables + reflect_schema
# =============================================================================

test_that("reflect_tables.mssql lists base tables in a schema", {
    engine <- mssql_test_engine()
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    con <- engine$get_connection()
    DBI::dbExecute(con, "DROP TABLE IF EXISTS dbo.alpha")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS dbo.beta")
    DBI::dbExecute(con, "CREATE TABLE dbo.alpha (id INT PRIMARY KEY)")
    DBI::dbExecute(con, "CREATE TABLE dbo.beta (id INT PRIMARY KEY)")

    tables <- oRm:::reflect_tables(engine, "dbo")
    expect_true(all(c("alpha", "beta") %in% tables))

    # Tables in other schemas are excluded.
    DBI::dbExecute(con, "IF SCHEMA_ID(N'other') IS NULL EXEC('CREATE SCHEMA other')")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS other.gamma")
    DBI::dbExecute(con, "CREATE TABLE other.gamma (id INT PRIMARY KEY)")
    expect_false("gamma" %in% oRm:::reflect_tables(engine, "dbo"))
    expect_true("gamma" %in% oRm:::reflect_tables(engine, "other"))
})

test_that("reflect_schema wires mssql relationships and traverses both ways", {
    engine <- mssql_test_engine()
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    con <- engine$get_connection()
    DBI::dbExecute(con, "DROP TABLE IF EXISTS dbo.posts")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS dbo.users")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS dbo.logs")
    DBI::dbExecute(con, "CREATE TABLE dbo.users (id INT IDENTITY(1,1) PRIMARY KEY, name NVARCHAR(100) NOT NULL)")
    DBI::dbExecute(con, paste(
        "CREATE TABLE dbo.posts (",
        "id INT IDENTITY(1,1) PRIMARY KEY,",
        "user_id INT NOT NULL REFERENCES dbo.users(id) ON DELETE CASCADE,",
        "title NVARCHAR(200))"
    ))
    DBI::dbExecute(con, "CREATE TABLE dbo.logs (id INT IDENTITY(1,1) PRIMARY KEY, msg NVARCHAR(200))")

    models <- engine$reflect_schema(tables = c("users", "posts"))

    expect_setequal(names(models), c("users", "posts"))
    expect_equal(models$posts$relationships$users$type, "many_to_one")
    expect_equal(models$users$relationships$posts$type, "one_to_many")

    Users <- models$users
    Posts <- models$posts
    Users$record(name = "Ada")$create()
    ada <- Users$read(name == "Ada", .mode = "one_or_none")
    Posts$record(user_id = ada$data$id, title = "Hello")$create()
    post <- Posts$read(title == "Hello", .mode = "one_or_none")

    expect_equal(post$relationship("users")$data$name, "Ada")
    expect_true(length(ada$relationship("posts")) >= 1)
})

test_that("reflect_schema warns and skips mssql foreign keys whose target is not reflected", {
    engine <- mssql_test_engine()
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    con <- engine$get_connection()
    DBI::dbExecute(con, "DROP TABLE IF EXISTS dbo.posts")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS dbo.users")
    DBI::dbExecute(con, "CREATE TABLE dbo.users (id INT IDENTITY(1,1) PRIMARY KEY, name NVARCHAR(100))")
    DBI::dbExecute(con, paste(
        "CREATE TABLE dbo.posts (",
        "id INT IDENTITY(1,1) PRIMARY KEY,",
        "user_id INT REFERENCES dbo.users(id))"
    ))

    expect_warning(
        models <- engine$reflect_schema(tables = "posts"),
        "not reflected"
    )
    expect_setequal(names(models), "posts")
    expect_length(models$posts$relationships, 0)
})

# =============================================================================
# Round trip: reflected definitions must regenerate valid T-SQL
# =============================================================================

test_that("a reflected mssql model regenerates a creatable table", {
    engine <- mssql_test_engine()
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    con <- engine$get_connection()
    DBI::dbExecute(con, "DROP TABLE IF EXISTS dbo.source_tbl")
    DBI::dbExecute(con, paste(
        "CREATE TABLE dbo.source_tbl (",
        "id INT IDENTITY(1,1) PRIMARY KEY,",
        "name NVARCHAR(100) NOT NULL,",
        "balance DECIMAL(10,2) NULL,",
        # Left nullable so the insert below can omit it and prove the reflected
        # DEFAULT is what fills it in.
        "active BIT NULL DEFAULT 1)"
    ))

    Source <- engine$reflect("source_tbl")

    # Recreate the same shape under a new name; if the rendered types, keys or
    # defaults were wrong this would raise.
    Copy <- do.call(engine$model, c(list("copy_tbl"), Source$fields))
    expect_no_error(Copy$create_table())

    Copy$record(name = "round trip")$create()
    row <- Copy$read(.mode = "data.frame")
    expect_equal(nrow(row), 1L)
    expect_equal(row$name, "round trip")
    # IDENTITY still generates, and the DEFAULT survived.
    expect_true(row$id >= 1)
    expect_true(as.logical(row$active))
})
