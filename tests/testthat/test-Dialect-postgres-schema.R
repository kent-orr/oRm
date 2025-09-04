testthat::source_test_helpers()

test_that("engine schema can be set on initialization", {
    conn_info <- tryCatch({
        c(setup_postgres_test_db(), .schema = "test")
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    expect_equal(engine$schema, "test")
    expect_equal(engine$conn_args$dbname, "tests")
    expect_equal(engine$conn_args$host, "localhost")
    expect_equal(engine$conn_args$user, "tester")
})

test_that("engine$create_schema works and is idempotent", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) skip(paste("Could not set up PostgreSQL container: ", e$message)))
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)

    expect_silent(engine$create_schema("somenew"))
    expect_silent(engine$create_schema("somenew")) # Should not error if schema already exists
    stats <- DBI::dbGetQuery(engine$get_connection(),
        "SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'somenew'")
    expect_equal(nrow(stats), 1)
})

test_that("check_schema_exists validates schemas", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    withr::defer(DBI::dbExecute(engine$get_connection(), "DROP SCHEMA IF EXISTS missing_schema CASCADE"))

    expect_false(engine$check_schema_exists("missing_schema"))
    expect_silent(engine$create_schema("missing_schema"))
    expect_true(engine$check_schema_exists("missing_schema"))
})

test_that("create_table fails if schema does not exist, succeeds after explicit engine$create_schema", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, c(conn_info, .schema = "not_exist"))
    withr::defer(engine$close())

    user_model <- engine$model(
        "users",
        id = Column("SERIAL", primary_key = TRUE),
        name = Column("TEXT", nullable = FALSE)
    )

    # Should error on create_table since schema does not exist
    expect_error(
        user_model$create_table(),
        "Schema 'not_exist' does not exist. Create it using engine\\$create_schema\\('not_exist'\\) before proceeding\\."
    )

    # Now explicitly create the schema
    expect_silent(engine$create_schema("not_exist"))

    # Now create_table should work
    expect_no_error(user_model$create_table(overwrite = TRUE))
    # And the table is now present in that schema
    conn <- engine$get_connection()
    res_table <- DBI::dbGetQuery(
        conn,
        "SELECT table_name FROM information_schema.tables WHERE table_schema = 'not_exist'"
    )
    expect_true("users" %in% res_table$table_name)
})

test_that("tables remain accessible across engine connections", {
    conn_info <- tryCatch({
        c(setup_postgres_test_db(), .schema = "test")
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    engine$create_schema("test")

    User <- engine$model(
        "users",
        id = Column("SERIAL", primary_key = TRUE),
        name = Column("TEXT", nullable = FALSE)
    )
    User$create_table(overwrite = TRUE)

    conn1 <- engine$get_connection()
    tables1 <- DBI::dbGetQuery(conn1, "SELECT tablename FROM pg_tables WHERE schemaname = 'test'")
    expect_true("users" %in% tables1$tablename)

    engine$close()
    conn2 <- engine$get_connection()
    tables2 <- DBI::dbGetQuery(conn2, "SELECT tablename FROM pg_tables WHERE schemaname = 'test'")
    expect_true("users" %in% tables2$tablename)

    expect_no_error(User$read(.mode = "all"))
})

test_that("engine creates models with default schema", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    UserPublic <- engine$model(
        "users",
        id = Column("SERIAL", primary_key = TRUE),
        name = Column("TEXT", nullable = FALSE)
    )
    UserPublic$create_table(overwrite = TRUE)
    withr::defer(UserPublic$drop_table(ask = FALSE))
    
    # Test that model uses unqualified table name when no schema is set
    expect_equal(UserPublic$tablename, "users")
    
    # Test that engine has no default schema
    expect_null(engine$schema)
    
    # Test that table is created in the default 'public' schema
    conn <- engine$get_connection()
    res_table <- DBI::dbGetQuery(conn, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public' AND table_name = 'users'")
    expect_equal(nrow(res_table), 1)
    expect_equal(res_table$table_name, "users")
    
    # Test that we can query the table directly without schema qualification
    table_exists <- DBI::dbExistsTable(conn, "users")
    expect_true(table_exists)
    
    # Test that we can execute operations on the unqualified table name
    count_result <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as count FROM users")
    expect_equal(count_result$count, 0)
})

test_that("models can create and read records in schema", {
    conn_info <- tryCatch({
        c(setup_postgres_test_db(), .schema = "test")
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    engine$create_schema("test")

    UserPublic <- engine$model(
        "users",
        id = Column("SERIAL", primary_key = TRUE),
        name = Column("TEXT", nullable = FALSE)
    )
    UserPublic$create_table(overwrite = TRUE)
    withr::defer(UserPublic$drop_table(ask = FALSE))
    rec_public <- UserPublic$record(name = "Alice")
    rec_public$create()
    res_public <- UserPublic$read(.mode = "all")
    expect_equal(length(res_public), 1)
    expect_equal(res_public[[1]]$data$name, "Alice")
})

test_that("models can override engine default schema", {
    conn_info <- tryCatch({
        c(setup_postgres_test_db(), .schema = "test")
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    UserArchive <- engine$model("users", .schema = "archive")
    expect_equal(UserArchive$tablename, "archive.users")
    expect_equal(engine$schema, "test")
})

test_that("engine set_schema changes search path", {
    conn_info <- tryCatch({
        c(setup_postgres_test_db(), .schema = "test")
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    engine$create_schema("test")

    DBI::dbExecute(engine$get_connection(), "CREATE SCHEMA IF NOT EXISTS audit")
    engine$set_schema("audit")
    engine$close()
    sp <- DBI::dbGetQuery(engine$get_connection(), "SHOW search_path")[[1]]
    expect_match(sp, 'audit')
})

test_that("model set_schema updates tablename", {
    conn_info <- tryCatch({
        c(setup_postgres_test_db(), .schema = "test")
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    engine$create_schema("test")

    DBI::dbExecute(engine$get_connection(), "CREATE SCHEMA IF NOT EXISTS audit")
    engine$set_schema("audit")

    UserArchive <- engine$model("users", .schema = "archive")
    UserArchive$set_schema(engine$schema)
    expect_equal(UserArchive$tablename, "audit.users")
})

test_that("models work across different schemas", {
    conn_info <- tryCatch({
        c(setup_postgres_test_db(), .schema = "test")
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    engine$create_schema("test")

    DBI::dbExecute(engine$get_connection(), "CREATE SCHEMA IF NOT EXISTS audit")
    engine$set_schema("audit")

    UserArchive <- engine$model(
        "users",
        id = Column("SERIAL", primary_key = TRUE),
        name = Column("TEXT", nullable = FALSE)
    )
    UserArchive$set_schema(engine$schema)
    UserArchive$create_table(overwrite = TRUE)
    withr::defer(UserArchive$drop_table(ask = FALSE))

    rec_audit <- UserArchive$record(name = "Bob")
    rec_audit$create()
    res_audit <- UserArchive$read(.mode = "all")
    expect_equal(length(res_audit), 1)
    expect_equal(res_audit[[1]]$data$name, "Bob")
})

test_that("record set_schema updates model tablename", {
    conn_info <- tryCatch({
        c(setup_postgres_test_db(), .schema = "test")
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    engine$create_schema("test")

    DBI::dbExecute(engine$get_connection(), "CREATE SCHEMA IF NOT EXISTS audit")
    engine$set_schema("audit")

    UserArchive <- engine$model(
        "users",
        id = Column("SERIAL", primary_key = TRUE),
        name = Column("TEXT", nullable = FALSE)
    )
    UserArchive$set_schema(engine$schema)
    UserArchive$create_table(overwrite = TRUE)
    withr::defer(UserArchive$drop_table(ask = FALSE))

    rec_audit <- UserArchive$record(name = "Bob")
    rec_audit$create()

    engine$set_schema("public")
    rec_audit$set_schema(engine$schema)
    expect_equal(rec_audit$model$tablename, "public.users")
})

