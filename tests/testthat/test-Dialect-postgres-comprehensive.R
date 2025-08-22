testthat::source_test_helpers()

# PostgreSQL Dialect-Specific Tests
# Tests the dialect methods defined in Dialect-postgres.R

test_that("flush.postgres returns data with RETURNING clause", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    # Create test table
    conn <- engine$get_connection()
    DBI::dbExecute(conn, "CREATE TABLE test_table (id SERIAL PRIMARY KEY, name TEXT, age INTEGER)")
    withr::defer(DBI::dbExecute(conn, "DROP TABLE IF EXISTS test_table"))
    
    # Test flush.postgres with RETURNING
    test_data <- list(name = "John", age = 25)
    result <- oRm:::flush.postgres(engine, "test_table", test_data, conn, commit = FALSE)
    
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 1)
    expect_true("id" %in% names(result))
    expect_equal(result$name, "John")
    expect_equal(result$age, 25)
    expect_equal(result$id, 1)
})

test_that("flush.postgres handles NULL values correctly", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    # Create test table
    conn <- engine$get_connection()
    DBI::dbExecute(conn, "CREATE TABLE test_table (id SERIAL PRIMARY KEY, name TEXT, age INTEGER)")
    withr::defer(DBI::dbExecute(conn, "DROP TABLE IF EXISTS test_table"))
    
    # Test flush.postgres with NULL values (should be filtered out)
    test_data <- list(name = "Jane", age = NULL, extra = NULL)
    result <- oRm:::flush.postgres(engine, "test_table", test_data, conn, commit = FALSE)
    
    expect_true(is.data.frame(result))
    expect_equal(result$name, "Jane")
    expect_true(is.na(result$age))
    expect_equal(result$id, 1)
})

test_that("qualify.postgres adds schema prefix correctly", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    
    # Test with schema
    qualified <- oRm:::qualify.postgres(engine, "users", "audit")
    expect_equal(qualified, "audit.users")
    
    # Test without schema
    unqualified <- oRm:::qualify.postgres(engine, "users", NULL)
    expect_equal(unqualified, "users")
    
    # Test with already qualified table
    already_qualified <- oRm:::qualify.postgres(engine, "public.users", "audit")
    expect_equal(already_qualified, "public.users")  # Should not add schema to already qualified
})

test_that("set_schema.postgres returns invisible NULL", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    
    # Test set_schema.postgres (it's a no-op that returns invisible NULL)
    result <- oRm:::set_schema.postgres(engine, "test_schema")
    expect_null(result)
})

test_that("create_schema.postgres creates schema and is idempotent", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    
    # Test creating a new schema
    expect_silent(oRm:::create_schema.postgres(engine, "test_schema"))
    
    # Verify schema was created
    conn <- engine$get_connection()
    schemas <- DBI::dbGetQuery(conn, "SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'test_schema'")
    expect_equal(nrow(schemas), 1)
    
    # Test idempotency - should not error when schema already exists
    expect_silent(oRm:::create_schema.postgres(engine, "test_schema"))
})

test_that("create_schema.postgres fails with NULL schema", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    
    expect_error(
        oRm:::create_schema.postgres(engine, NULL),
        "Must supply a schema name."
    )
})

test_that("ensure_schema_exists.postgres validates schema existence", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    
    # Test with NULL schema (should return invisible NULL)
    expect_invisible(oRm:::ensure_schema_exists.postgres(engine, NULL))
    
    # Test with existing schema (public should exist)
    expect_invisible(oRm:::ensure_schema_exists.postgres(engine, "public"))
    
    # Test with non-existing schema
    expect_error(
        oRm:::ensure_schema_exists.postgres(engine, "nonexistent_schema"),
        "Schema 'nonexistent_schema' does not exist. Create it using engine\\$create_schema\\('nonexistent_schema'\\) before proceeding\\."
    )
    
    # Create schema and test again
    oRm:::create_schema.postgres(engine, "test_exists")
    expect_invisible(oRm:::ensure_schema_exists.postgres(engine, "test_exists"))
})

test_that("ensure_schema_exists.postgres works with TableModel", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    
    # Create a table model
    TestModel <- engine$model(
        "test_table",
        id = Column("SERIAL", primary_key = TRUE),
        name = Column("TEXT")
    )
    
    # Test ensure_schema_exists with TableModel and existing schema
    expect_invisible(oRm:::ensure_schema_exists.postgres(TestModel, "public"))
    
    # Test with non-existing schema
    expect_error(
        oRm:::ensure_schema_exists.postgres(TestModel, "nonexistent_from_model"),
        "Schema 'nonexistent_from_model' does not exist"
    )
})

test_that("ensure_schema_exists.postgres handles connection properly", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    
    # Close the connection to test connection handling
    engine$close()
    
    # ensure_schema_exists should handle creating its own connection
    expect_invisible(oRm:::ensure_schema_exists.postgres(engine, "public"))
})

test_that("PostgreSQL dialect method dispatch works correctly", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    
    # Test that dispatch_method calls postgres-specific functions
    expect_equal(engine$dialect, "postgres")
    
    # Test qualify dispatch
    qualified <- oRm:::qualify(engine, "users", "audit")
    expect_equal(qualified, "audit.users")
    
    # Test set_schema dispatch
    result <- oRm:::set_schema(engine, "test")
    expect_null(result)
    
    # Test create_schema dispatch
    expect_silent(oRm:::create_schema(engine, "dispatch_test"))
    
    # Test ensure_schema_exists dispatch
    expect_invisible(oRm:::ensure_schema_exists(engine, "dispatch_test"))
})

test_that("PostgreSQL dialect handles special characters in data", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    
    # Create test table
    conn <- engine$get_connection()
    DBI::dbExecute(conn, "CREATE TABLE test_special (id SERIAL PRIMARY KEY, text_field TEXT)")
    withr::defer(DBI::dbExecute(conn, "DROP TABLE IF EXISTS test_special"))
    
    # Test with special characters
    special_data <- list(text_field = "Test with 'quotes' and \"double quotes\" and \\ backslash")
    result <- oRm:::flush.postgres(engine, "test_special", special_data, conn, commit = FALSE)
    
    expect_equal(result$text_field, special_data$text_field)
    expect_equal(result$id, 1)
})

test_that("PostgreSQL dialect handles different data types correctly", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    
    # Create test table with various data types
    conn <- engine$get_connection()
    DBI::dbExecute(conn, "CREATE TABLE test_types (
        id SERIAL PRIMARY KEY,
        text_col TEXT,
        int_col INTEGER,
        bool_col BOOLEAN,
        date_col DATE,
        timestamp_col TIMESTAMP
    )")
    withr::defer(DBI::dbExecute(conn, "DROP TABLE IF EXISTS test_types"))
    
    # Test with various data types
    test_data <- list(
        text_col = "test text",
        int_col = 42L,
        bool_col = TRUE,
        date_col = as.Date("2023-01-01"),
        timestamp_col = as.POSIXct("2023-01-01 12:00:00", tz = "UTC")
    )
    
    result <- oRm:::flush.postgres(engine, "test_types", test_data, conn, commit = FALSE)
    
    expect_equal(result$text_col, "test text")
    expect_equal(result$int_col, 42L)
    expect_equal(result$bool_col, TRUE)
    expect_equal(as.character(result$date_col), "2023-01-01")
    expect_equal(result$id, 1)
})

test_that("PostgreSQL flush method respects transaction state", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    
    # Create test table
    conn <- engine$get_connection()
    DBI::dbExecute(conn, "CREATE TABLE test_transaction (id SERIAL PRIMARY KEY, name TEXT)")
    withr::defer(DBI::dbExecute(conn, "DROP TABLE IF EXISTS test_transaction"))
    
    # Test that flush.postgres doesn't commit when commit = FALSE
    DBI::dbBegin(conn)
    test_data <- list(name = "test_user")
    result <- oRm:::flush.postgres(engine, "test_transaction", test_data, conn, commit = FALSE)
    
    # Data should be inserted but not committed
    expect_equal(result$name, "test_user")
    expect_equal(result$id, 1)
    
    # Rollback to verify it wasn't committed
    DBI::dbRollback(conn)
    
    # Verify data was rolled back
    count_result <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as count FROM test_transaction")
    expect_equal(count_result$count, 0)
})

test_that("PostgreSQL dialect integration with Engine CRUD operations", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    
    # Create test model
    TestModel <- engine$model(
        "integration_test",
        id = Column("SERIAL", primary_key = TRUE),
        name = Column("TEXT", nullable = FALSE),
        value = Column("INTEGER")
    )
    
    TestModel$create_table(overwrite = TRUE)
    withr::defer(TestModel$drop_table(ask = FALSE))
    
    # Test that Record create() uses PostgreSQL RETURNING
    record <- TestModel$record(name = "integration_test", value = 100)
    record$create()
    
    # Should have ID populated from RETURNING clause
    expect_equal(record$data$id, 1)
    expect_equal(record$data$name, "integration_test")
    expect_equal(record$data$value, 100)
    
    # Verify data in database
    db_data <- DBI::dbGetQuery(engine$get_connection(), "SELECT * FROM integration_test WHERE id = 1")
    expect_equal(nrow(db_data), 1)
    expect_equal(db_data$name, "integration_test")
    expect_equal(db_data$value, 100)
})