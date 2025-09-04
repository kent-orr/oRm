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
    expect_equal(conn_info$dbname, "test")
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
    
    # Connection should now fail
    expect_error(
        do.call(DBI::dbConnect, conn_info),
        "Connection refused|could not connect"
    )
})

test_that("postgres helpers skip appropriately when docker unavailable", {
    # Mock docker availability
    if (requireNamespace("stevedore", quietly = TRUE)) {
        testthat::with_mocked_bindings(
            docker_available = function() FALSE,
            .package = "stevedore",
            expect_condition(
                setup_postgres_test_db(),
                class = "skip"
            )
        )
    } else {
        expect_condition(
            setup_postgres_test_db(),
            class = "skip"
        )
    }
})

test_that("postgres helpers skip appropriately when packages missing", {
    # This will naturally skip if stevedore or RPostgres aren't available
    # due to the testthat::skip_if_not_installed calls
    expect_no_error({
        tryCatch({
            setup_postgres_test_db()
            cleanup_postgres_test_db()
        }, error = function(e) {
            # Expected if Docker or packages unavailable
            if (grepl("skip|not available|not installed", e$message, ignore.case = TRUE)) {
                testthat::skip(e$message)
            } else {
                stop(e)
            }
        })
    })
})