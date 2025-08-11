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
    expect_equal(engine$conn_args$dbname, "test")
    expect_equal(engine$conn_args$schema, "test")
    expect_equal(engine$conn_args$host, "localhost")
    expect_equal(engine$conn_args$user, "tester")
})

test_that("engine models create a schema if it does not exist", {
    conn_info <- tryCatch({
        c(setup_postgres_test_db(), .schema = "test")
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    model <- engine$model("users", id = Column("SERIAL", primary_key = TRUE), name = Column("TEXT", nullable = FALSE))
    model$create_table(overwrite = TRUE)
    expect_equal(model$tablename, "test.users")

    engine$list_tables()
    conn <- engine$get_connection()
    res <- DBI::dbGetQuery(conn, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'test'")

    engine$execute("INSERT INTO test.users (name) VALUES ('Alice')")
})

test_that("engine schema operations work with Postgres", {
    conn_info <- tryCatch({
        c(setup_postgres_test_db(), .schema = "test")
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
    expect_equal(UserPublic$tablename, "public.users")

    rec_public <- UserPublic$record(name = "Alice")
    rec_public$create()
    res_public <- UserPublic$read(mode = "all")
    expect_equal(length(res_public), 1)
    expect_equal(res_public[[1]]$data$name, "Alice")

    UserArchive <- engine$model("users", .schema = "archive")
    expect_equal(UserArchive$tablename, "archive.users")
    expect_equal(engine$schema, "public")

    DBI::dbExecute(engine$get_connection(), "CREATE SCHEMA IF NOT EXISTS audit")
    engine$set_schema("audit")
    engine$close()
    sp <- DBI::dbGetQuery(engine$get_connection(), "SHOW search_path")[[1]]
    expect_match(sp, '"audit"')
    UserArchive$set_schema(engine$schema)
    expect_equal(UserArchive$tablename, "audit.users")

    UserArchive$create_table(overwrite = TRUE)
    rec_audit <- UserArchive$record(name = "Bob")
    rec_audit$create()
    res_audit <- UserArchive$read(mode = "all")
    expect_equal(length(res_audit), 1)
    expect_equal(res_audit[[1]]$data$name, "Bob")

    engine$set_schema("public")
    rec_audit$set_schema(engine$schema)
    expect_equal(rec_audit$model$tablename, "public.users")

    UserArchive$drop_table(ask = FALSE)
    UserPublic$drop_table(ask = FALSE)
})

