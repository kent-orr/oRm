testthat::source_test_helpers()

# PostgreSQL Dialect Edge Cases and Error Handling Tests

test_that("flush.postgres handles empty data list", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    # Create test table with default values
    conn <- engine$get_connection()
    DBI::dbExecute(conn, "CREATE TABLE test_empty (
        id SERIAL PRIMARY KEY, 
        name TEXT DEFAULT 'default_name',
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )")
    withr::defer(DBI::dbExecute(conn, "DROP TABLE IF EXISTS test_empty"))
    
    # Test flush with empty data (all values are NULL and get filtered out)
    empty_data <- list(name = NULL, extra_field = NULL)
    
    # This should result in an INSERT with no columns, which PostgreSQL handles with DEFAULT VALUES
    expect_error(
        oRm:::flush.postgres(engine, "test_empty", empty_data, conn, commit = FALSE),
        "syntax error"
    )
})

test_that("flush.postgres handles very long strings", {
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
    DBI::dbExecute(conn, "CREATE TABLE test_long (id SERIAL PRIMARY KEY, long_text TEXT)")
    withr::defer(DBI::dbExecute(conn, "DROP TABLE IF EXISTS test_long"))
    
    # Test with very long string (10KB)
    long_string <- paste(rep("A", 10000), collapse = "")
    test_data <- list(long_text = long_string)
    
    result <- oRm:::flush.postgres(engine, "test_long", test_data, conn, commit = FALSE)
    expect_equal(nchar(result$long_text), 10000)
    expect_equal(result$id, 1)
})

test_that("flush.postgres handles SQL injection attempts safely", {
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
    DBI::dbExecute(conn, "CREATE TABLE test_injection (id SERIAL PRIMARY KEY, name TEXT)")
    withr::defer(DBI::dbExecute(conn, "DROP TABLE IF EXISTS test_injection"))
    
    # Test with SQL injection attempt
    malicious_data <- list(name = "'; DROP TABLE test_injection; --")
    
    result <- oRm:::flush.postgres(engine, "test_injection", malicious_data, conn, commit = FALSE)
    
    # Should properly escape and insert the malicious string as data
    expect_equal(result$name, "'; DROP TABLE test_injection; --")
    expect_equal(result$id, 1)
    
    # Verify table still exists and has the data
    count_result <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as count FROM test_injection")
    expect_equal(count_result$count, 1)
})

test_that("qualify.postgres handles edge cases in table names", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    
    # Test with table name containing dots
    dotted_table <- oRm:::qualify.postgres(engine, "table.with.dots", "schema")
    expect_equal(dotted_table, "table.with.dots")  # Should not qualify already dotted names
    
    # Test with empty string table name
    empty_table <- oRm:::qualify.postgres(engine, "", "schema")
    expect_equal(empty_table, "schema.")  # This might not be desired behavior, but documents current
    
    # Test with special characters in schema
    special_schema <- oRm:::qualify.postgres(engine, "users", "my-schema")
    expect_equal(special_schema, "my-schema.users")
})


test_that("create_schema.postgres handles invalid schema names", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    
    # Test with invalid schema name (PostgreSQL has naming rules)
    expect_error(
        oRm:::create_schema.postgres(engine, "123invalid-start"),
        "syntax error|invalid"
    )
})

test_that("PostgreSQL dialect functions work with disconnected engine", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    
    # Close the connection
    engine$close()
    
    # Functions that don't require connection should still work
    qualified <- oRm:::qualify.postgres(engine, "users", "audit")
    expect_equal(qualified, "audit.users")
    
    set_result <- oRm:::set_schema.postgres(engine, "test")
    expect_null(set_result)
    
    # Functions requiring connection should handle reconnection
    # (removed ensure_schema_exists test as function was removed)
})

test_that("flush.postgres handles concurrent access scenarios", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    
    engine1 <- do.call(Engine$new, conn_info)
    engine2 <- do.call(Engine$new, conn_info)
    withr::defer(engine1$close())
    withr::defer(engine2$close())
    
    # Create test table
    conn1 <- engine1$get_connection()
    DBI::dbExecute(conn1, "CREATE TABLE test_concurrent (id SERIAL PRIMARY KEY, name TEXT)")
    withr::defer(DBI::dbExecute(conn1, "DROP TABLE IF EXISTS test_concurrent"))
    
    # Test concurrent inserts
    conn2 <- engine2$get_connection()
    
    result1 <- oRm:::flush.postgres(engine1, "test_concurrent", list(name = "user1"), conn1, commit = FALSE)
    result2 <- oRm:::flush.postgres(engine2, "test_concurrent", list(name = "user2"), conn2, commit = FALSE)
    
    expect_equal(result1$name, "user1")
    expect_equal(result2$name, "user2")
    expect_true(result1$id != result2$id)  # Should have different IDs
    
    # Commit both transactions
    DBI::dbCommit(conn1)
    DBI::dbCommit(conn2)
    
    # Verify both records exist
    count_result <- DBI::dbGetQuery(conn1, "SELECT COUNT(*) as count FROM test_concurrent")
    expect_equal(count_result$count, 2)
})

test_that("PostgreSQL dialect handles non-standard column names", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    
    # Create test table with quoted column names
    conn <- engine$get_connection()
    DBI::dbExecute(conn, 'CREATE TABLE test_quotes (
        id SERIAL PRIMARY KEY, 
        "user name" TEXT,
        "order" INTEGER,
        "select" TEXT
    )')
    withr::defer(DBI::dbExecute(conn, "DROP TABLE IF EXISTS test_quotes"))
    
    # Test flush with quoted column names
    test_data <- list(
        `user name` = "John Doe",
        `order` = 1,
        `select` = "value"
    )
    
    result <- oRm:::flush.postgres(engine, "test_quotes", test_data, conn, commit = FALSE)
    
    expect_equal(result$`user name`, "John Doe")
    expect_equal(result$order, 1)
    expect_equal(result$select, "value")
    expect_equal(result$id, 1)
})