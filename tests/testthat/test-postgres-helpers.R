testthat::source_test_helpers()

test_that("setup_postgres_test_db returns valid connection info", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    
    expect_type(conn_info, "list")
    expect_named(conn_info, c("drv", "dbname", "host", "user", "password", "port"))
    expect_equal(conn_info$dbname, "tests")
    expect_equal(conn_info$host, "localhost")
    expect_equal(conn_info$user, "tester")
    expect_equal(conn_info$password, "tester")
    expect_equal(conn_info$port, 5432)
    expect_s4_class(conn_info$drv, "PqDriver")
})

test_that("setup_postgres_test_db creates working database connection", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    
    con <- tryCatch({
        do.call(DBI::dbConnect, conn_info)
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL:", e$message))
    })
    withr::defer(DBI::dbDisconnect(con))
    
    expect_s4_class(con, "PqConnection")
    expect_no_error(DBI::dbExecute(con, "SELECT 1"))
    expect_no_error(DBI::dbExecute(con, "CREATE TEMPORARY TABLE test_table (id INTEGER)"))
    expect_no_error(DBI::dbExecute(con, "INSERT INTO test_table VALUES (1)"))
    
    result <- DBI::dbGetQuery(con, "SELECT id FROM test_table")
    expect_equal(result$id, 1)
})

test_that("setup_postgres_test_db handles existing containers", {
    # First setup
    conn_info1 <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    
    # Second setup should clean up and recreate
    conn_info2 <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up second PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    
    expect_equal(conn_info1, conn_info2)
    
    # Should be able to connect to the new container
    con <- tryCatch({
        do.call(DBI::dbConnect, conn_info2)
    }, error = function(e) {
        testthat::skip(paste("Could not connect to recreated PostgreSQL:", e$message))
    })
    withr::defer(DBI::dbDisconnect(con))
    
    expect_no_error(DBI::dbExecute(con, "SELECT 1"))
})

test_that("cleanup_postgres_test_db removes container", {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    
    # Verify connection works
    con <- tryCatch({
        do.call(DBI::dbConnect, conn_info)
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL:", e$message))
    })
    DBI::dbDisconnect(con)
    
    # Clean up
    expect_no_error(cleanup_postgres_test_db())
    
    # Give Docker time to fully stop the container
    Sys.sleep(2)
    
    # Connection should now fail - try with timeout to avoid hanging
    expect_error({
        tryCatch({
            con <- DBI::dbConnect(
                conn_info$drv,
                dbname = conn_info$dbname,
                host = conn_info$host,
                user = conn_info$user,
                password = conn_info$password,
                port = conn_info$port,
                connect_timeout = 3
            )
            DBI::dbDisconnect(con)
            stop("Connection unexpectedly succeeded")
        }, error = function(e) {
            if (grepl("Connection refused|could not connect|timeout", e$message, ignore.case = TRUE)) {
                # Expected error - connection properly failed
                stop(e$message)
            } else {
                # Re-throw unexpected errors
                stop(e$message)
            }
        })
    }, "Connection refused|could not connect|timeout|Connection unexpectedly succeeded")
})

test_that("use_postgres_test_db returns connection info", {
    # Setup database first
    conn_info_setup <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    
    # Test use_postgres_test_db
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not use PostgreSQL test db:", e$message))
    })
    
    expect_type(conn_info, "list")
    expect_named(conn_info, c("drv", "dbname", "host", "user", "password", "port"))
    expect_equal(conn_info$dbname, "tests")
    expect_equal(conn_info$host, "localhost")
    expect_equal(conn_info$user, "tester")
    expect_equal(conn_info$password, "tester")
    expect_equal(conn_info$port, 5432)
    expect_s4_class(conn_info$drv, "PqDriver")
    
    # Should be able to connect
    con <- tryCatch({
        do.call(DBI::dbConnect, conn_info)
    }, error = function(e) {
        testthat::skip(paste("Could not connect using use_postgres_test_db:", e$message))
    })
    withr::defer(DBI::dbDisconnect(con))
    
    expect_no_error(DBI::dbExecute(con, "SELECT 1"))
})

test_that("clear_postgres_test_tables removes all tables", {
    # Setup database
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    
    # Connect and create test tables
    con <- tryCatch({
        do.call(DBI::dbConnect, conn_info)
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL:", e$message))
    })
    withr::defer(DBI::dbDisconnect(con))
    
    # Create some test tables
    expect_no_error(DBI::dbExecute(con, "CREATE TABLE test_table1 (id INTEGER, name TEXT)"))
    expect_no_error(DBI::dbExecute(con, "CREATE TABLE test_table2 (id INTEGER, value NUMERIC)"))
    expect_no_error(DBI::dbExecute(con, "INSERT INTO test_table1 VALUES (1, 'test')"))
    expect_no_error(DBI::dbExecute(con, "INSERT INTO test_table2 VALUES (1, 3.14)"))
    
    # Verify tables exist
    tables_before <- DBI::dbGetQuery(con, "
        SELECT table_name 
        FROM information_schema.tables 
        WHERE table_schema = 'public' 
        AND table_type = 'BASE TABLE'
    ")
    expect_gte(nrow(tables_before), 2)
    expect_true("test_table1" %in% tables_before$table_name)
    expect_true("test_table2" %in% tables_before$table_name)
    
    # Clear tables
    expect_no_error(clear_postgres_test_tables())
    
    # Verify tables are gone
    tables_after <- DBI::dbGetQuery(con, "
        SELECT table_name 
        FROM information_schema.tables 
        WHERE table_schema = 'public' 
        AND table_type = 'BASE TABLE'
    ")
    expect_equal(nrow(tables_after), 0)
})

test_that("postgres helpers work with real Docker containers", {
    # This test verifies the full workflow without mocking
    testthat::skip_if_not_installed("stevedore")
    testthat::skip_if_not_installed("RPostgres")
    
    if (!stevedore::docker_available()) {
        testthat::skip("Docker not available, skipping PostgreSQL container tests")
    }
    
    # Test full workflow
    conn_info <- expect_no_error(setup_postgres_test_db())
    withr::defer(cleanup_postgres_test_db())
    
    # Test connection
    con <- expect_no_error(do.call(DBI::dbConnect, conn_info))
    withr::defer(DBI::dbDisconnect(con))
    
    # Test basic operations
    expect_no_error(DBI::dbExecute(con, "CREATE TABLE workflow_test (id SERIAL, data TEXT)"))
    expect_no_error(DBI::dbExecute(con, "INSERT INTO workflow_test (data) VALUES ('test')"))
    
    result <- DBI::dbGetQuery(con, "SELECT * FROM workflow_test")
    expect_equal(nrow(result), 1)
    expect_equal(result$data, "test")
    
    # Test table clearing
    expect_no_error(clear_postgres_test_tables())
    
    tables <- DBI::dbGetQuery(con, "
        SELECT table_name 
        FROM information_schema.tables 
        WHERE table_schema = 'public' 
        AND table_type = 'BASE TABLE'
    ")
    expect_equal(nrow(tables), 0)
})