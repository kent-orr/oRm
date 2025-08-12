testthat::source_test_helpers()

test_that("postgres engine initializes and closes", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    expect_equal(engine$dialect, "postgres")
    expect_no_error(engine$get_connection())
    expect_no_error(engine$close())
})

test_that("postgres create operations work", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    TempUser <- engine$model(
        "temp_users",
        id = Column("SERIAL", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE),
        age = Column("INTEGER")
    )

    TempUser$create_table(overwrite = TRUE)
    withr::defer(TempUser$drop_table(ask = FALSE))
    expect_true("temp_users" %in% engine$list_tables())

    p1 <- TempUser$record(name = "John", age = 18)
    p1$create()
    expect_equal(p1$data$id, 1)

    p2 <- TempUser$record(name = "Jane", age = 25)
    p2$create()
    expect_equal(p2$data$id, 2)

    all_users <- TempUser$read(mode = "all")
    expect_equal(length(all_users), 2)
})

test_that("postgres read operations work", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    TempUser <- engine$model(
        "temp_users",
        id = Column("SERIAL", primary_key = TRUE),
        name = Column("TEXT", nullable = FALSE),
        age = Column("INTEGER")
    )

    TempUser$create_table(overwrite = TRUE)
    withr::defer(TempUser$drop_table(ask = FALSE))

    p1 <- TempUser$record(id = 1, name = "test_person", age = 19)
    p1$create()
    p1_user <- TempUser$read(id == 1, mode = "get")
    expect_equal(p1, p1_user)
})

test_that("postgres update/refresh works", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    TempUser <- engine$model(
        "temp_users",
        id = Column("SERIAL", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE),
        age = Column("INTEGER")
    )

    TempUser$create_table(overwrite = TRUE)
    withr::defer(TempUser$drop_table(ask = FALSE))

    p1 <- TempUser$record(name = "John", age = 18)
    p1$create()

    engine$execute("UPDATE temp_users SET age = 30 WHERE id = 1")
    p1$refresh()
    db_user <- engine$get_query("SELECT * FROM temp_users WHERE id = 1")
    expect_equal(p1$data, as.list(db_user[1, ]))
})

test_that("postgres delete operations work", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    TempUser <- engine$model(
        "temp_users",
        id = Column("SERIAL", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE),
        age = Column("INTEGER")
    )

    TempUser$create_table(overwrite = TRUE)
    withr::defer(TempUser$drop_table(ask = FALSE))

    p1 <- TempUser$record(name = "John", age = 18)
    p1$create()
    p2 <- TempUser$record(name = "Jane", age = 25)
    p2$create()

    p1$delete()
    expect_equal(length(TempUser$read(id == 1, mode = "all")), 0)
    remaining_users <- TempUser$read(mode = "all")
    expect_equal(length(remaining_users), 1)
})

test_that("postgres schema switching works", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    
    DBI::dbExecute(engine$get_connection(), "CREATE SCHEMA IF NOT EXISTS audit")
    withr::defer(DBI::dbExecute(engine$get_connection(), "DROP SCHEMA IF EXISTS audit CASCADE"))

    engine$set_schema("audit")
    withr::defer(engine$set_schema("public"))

    AuditUser <- engine$model(
        "audit_users",
        id = Column("SERIAL", primary_key = TRUE),
        name = Column("TEXT", nullable = FALSE)
    )

    AuditUser$create_table(overwrite = TRUE)
    withr::defer(AuditUser$drop_table(ask = FALSE))
    expect_true("audit.audit_users" %in% engine$list_tables())
})

