testthat::source_test_helpers()

# Suite-level cleanup to remove any leftover containers
withr::defer({
    cleanup_postgres_test_db()
}, testthat::teardown_env())

# Helper: a connected postgres engine, or skip.
pg_engine <- function() {
    conn_info <- tryCatch(
        use_postgres_test_db(),
        error = function(e) testthat::skip(paste("No PostgreSQL test database:", e$message))
    )
    do.call(Engine$new, conn_info)
}

# =============================================================================
# reflect_columns.postgres: types, keys, nullability, defaults, foreign keys
# =============================================================================

test_that("reflect_columns.postgres captures types, PK, nullability, defaults, and FKs", {
    engine <- pg_engine()
    withr::defer(clear_postgres_test_tables())
    withr::defer(engine$close())

    con <- engine$get_connection()
    DBI::dbExecute(con, "DROP TABLE IF EXISTS posts CASCADE")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS users CASCADE")
    DBI::dbExecute(con, "CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT NOT NULL, active BOOLEAN DEFAULT true)")
    DBI::dbExecute(con, paste(
        "CREATE TABLE posts (id SERIAL PRIMARY KEY,",
        "user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,",
        "title TEXT)"
    ))

    Users <- engine$reflect("users")
    uf <- Users$fields
    expect_true(isTRUE(uf$id$primary_key))
    expect_match(tolower(uf$id$type), "int")
    expect_false(isTRUE(uf$name$nullable))            # NOT NULL
    expect_true(isTRUE(uf$active$nullable))           # nullable
    expect_false(is.null(uf$active$default))          # DEFAULT true captured

    Posts <- engine$reflect("posts")
    pf <- Posts$fields
    expect_s3_class(pf$user_id, "ForeignKey")
    expect_equal(pf$user_id$ref_schema, "public")
    expect_equal(pf$user_id$ref_table, "users")
    expect_equal(pf$user_id$ref_column, "id")
    expect_equal(toupper(pf$user_id$on_delete), "CASCADE")
    expect_false(isTRUE(pf$user_id$nullable))         # NOT NULL FK
    expect_true(isTRUE(pf$title$nullable))
})

# =============================================================================
# Cross-schema foreign keys
# =============================================================================

test_that("reflect_columns.postgres captures cross-schema foreign keys", {
    engine <- pg_engine()
    withr::defer(clear_postgres_test_tables())
    withr::defer(engine$close())

    con <- engine$get_connection()
    DBI::dbExecute(con, "DROP TABLE IF EXISTS articles CASCADE")
    DBI::dbExecute(con, "DROP SCHEMA IF EXISTS other CASCADE")
    DBI::dbExecute(con, "CREATE SCHEMA other")
    withr::defer(DBI::dbExecute(engine$get_connection(), "DROP SCHEMA IF EXISTS other CASCADE"))
    DBI::dbExecute(con, "CREATE TABLE other.authors (id SERIAL PRIMARY KEY, name TEXT)")
    DBI::dbExecute(con, paste(
        "CREATE TABLE articles (id SERIAL PRIMARY KEY,",
        "author_id INTEGER REFERENCES other.authors(id))"
    ))

    Articles <- engine$reflect("articles")
    fk <- Articles$fields$author_id
    expect_s3_class(fk, "ForeignKey")
    expect_equal(fk$ref_schema, "other")
    expect_equal(fk$ref_table, "authors")
    expect_equal(fk$ref_column, "id")
    expect_equal(fk$references, "other.authors.id")
})

# =============================================================================
# reflect_schema: table limiting + relationship wiring + traversal
# =============================================================================

test_that("reflect_schema reflects a limited set and wires relationships", {
    engine <- pg_engine()
    withr::defer(clear_postgres_test_tables())
    withr::defer(engine$close())

    con <- engine$get_connection()
    DBI::dbExecute(con, "DROP TABLE IF EXISTS posts CASCADE")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS users CASCADE")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS logs CASCADE")
    DBI::dbExecute(con, "CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT NOT NULL)")
    DBI::dbExecute(con, paste(
        "CREATE TABLE posts (id SERIAL PRIMARY KEY,",
        "user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,",
        "title TEXT)"
    ))
    DBI::dbExecute(con, "CREATE TABLE logs (id SERIAL PRIMARY KEY, msg TEXT)")

    models <- engine$reflect_schema(tables = c("users", "posts"))

    # 'tables' limits the reflected set
    expect_setequal(names(models), c("users", "posts"))
    expect_false("logs" %in% names(models))

    # FK -> many_to_one relationship, with the one_to_many backref
    expect_true("users" %in% names(models$posts$relationships))
    expect_equal(models$posts$relationships$users$type, "many_to_one")
    expect_true("posts" %in% names(models$users$relationships))
    expect_equal(models$users$relationships$posts$type, "one_to_many")

    # End-to-end traversal in both directions
    Users <- models$users
    Posts <- models$posts
    Users$record(name = "Ada")$create()
    ada <- Users$read(name == "Ada", .mode = "one_or_none")
    Posts$record(user_id = ada$data$id, title = "Hello")$create()
    post <- Posts$read(title == "Hello", .mode = "one_or_none")

    author <- post$relationship("users")
    expect_equal(author$data$name, "Ada")

    ada_posts <- ada$relationship("posts")
    expect_true(length(ada_posts) >= 1)
})

test_that("reflect_schema warns and skips foreign keys whose target is not reflected", {
    engine <- pg_engine()
    withr::defer(clear_postgres_test_tables())
    withr::defer(engine$close())

    con <- engine$get_connection()
    DBI::dbExecute(con, "DROP TABLE IF EXISTS posts CASCADE")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS users CASCADE")
    DBI::dbExecute(con, "CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT)")
    DBI::dbExecute(con, paste(
        "CREATE TABLE posts (id SERIAL PRIMARY KEY,",
        "user_id INTEGER REFERENCES users(id))"
    ))

    # Hydrate only 'posts'; its FK target 'users' is outside the set.
    expect_warning(
        models <- engine$reflect_schema(tables = "posts"),
        "not reflected"
    )
    expect_setequal(names(models), "posts")
    expect_length(models$posts$relationships, 0)
})
