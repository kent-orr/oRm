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
    expect_equal(engine$conn_args$host, "localhost")
    expect_equal(engine$conn_args$user, "tester")
})

test_that("model create_table creates missing schema and table", {
    conn_info <- tryCatch({
        c(setup_postgres_test_db(), .schema = "test")
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    User <- engine$model(
        "users",
        id = Column("SERIAL", primary_key = TRUE),
        name = Column("TEXT", nullable = FALSE)
    )
    User$create_table(overwrite = TRUE)

    conn <- engine$get_connection()
    res_schema <- DBI::dbGetQuery(conn, "SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'test'")
    expect_equal(nrow(res_schema), 1)
    res_table <- DBI::dbGetQuery(conn, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'test'")
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

    expect_no_error(User$read(mode = "all"))
})



test_that('create_table creates schema when model schema changes', {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        skip(paste("Could not set up PostgreSQL container:", e$message))
        NULL
    })
    if (is.null(conn_info)) {
        skip("PostgreSQL container setup failed")
    }

    engine <- do.call(Engine$new, conn_info)

    model <- engine$model("users", id = Column("SERIAL", primary_key = TRUE))
    model$set_schema("audit")
    model$create_table(overwrite = TRUE)

    conn <- engine$get_connection()
    res_schema <- DBI::dbGetQuery(conn, "SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'audit'")
    expect_equal(nrow(res_schema), 1)
    res_table <- DBI::dbGetQuery(conn, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'audit'")
    expect_true("users" %in% res_table$table_name)

    cleanup_postgres_test_db()
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

