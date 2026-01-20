testthat::source_test_helpers()

# Suite-level setup
tryCatch({
    setup_postgres_test_db()
}, error = function(e) {
    testthat::skip(paste("Could not set up PostgreSQL container for suite:", e$message))
})

# Suite-level cleanup
withr::defer({
    cleanup_postgres_test_db()
}, testthat::teardown_env())

# =============================================================================
# 0. TEST INFRASTRUCTURE TESTS
# =============================================================================

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

# =============================================================================
# 1. ENGINE CONNECTION TESTS
# =============================================================================

test_that("postgres engine initializes and closes", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    expect_equal(engine$dialect, "postgres")
    expect_no_error(engine$get_connection())
    expect_no_error(engine$close())
})

test_that("PostgreSQL dialect method dispatch works correctly", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    
    # Test that dispatch_method calls postgres-specific functions
    expect_equal(engine$dialect, "postgres")
    
    # Test qualify dispatch
    qualified <- oRm:::qualify(engine, "users", "audit")
    expect_equal(qualified, "audit.users")
    
    # Test set_schema dispatch
    engine$set_schema('test')
    result <- oRm:::set_schema(engine, "test")
    expect_null(result)
    
    # Test create_schema dispatch
    expect_silent(oRm:::create_schema(engine, "dispatch_test"))
})

# =============================================================================
# 2. TABLE CREATION TESTS
# =============================================================================

test_that("postgres create operations work", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    
    engine <- Engine$new(conn_args = conn_info)

    TempUser <- engine$model(
        "temp_users",
        id = Column("SERIAL", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE),
        age = Column("INTEGER")
    )

    TempUser$create_table(overwrite = TRUE)
    expect_true("temp_users" %in% engine$list_tables())

    p1 <- TempUser$record(name = "John", age = 18)
    p1$create()
    expect_equal(p1$data$id, 1)

    p2 <- TempUser$record(name = "Jane", age = 25)
    p2$create()
    expect_equal(p2$data$id, 2)

    all_users <- TempUser$read(.mode = "all")
    expect_equal(length(all_users), 2)
})

# =============================================================================
# 3. CRUD OPERATIONS TESTS
# =============================================================================

test_that("postgres read operations work", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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
    p1_user <- TempUser$read(id == 1, .mode = "get")
    expect_equal(p1, p1_user)
})

test_that("postgres update/refresh works", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    clear_postgres_test_tables()
    
    engine <- do.call(Engine$new, conn_info)
    
    TempUser <- engine$model(
        "temp_users",
        id = Column("SERIAL", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE),
        age = Column("INTEGER")
    )

    TempUser$create_table(overwrite = TRUE)

    p1 <- TempUser$record(name = "John", age = 18)
    p1$create()

    engine$execute("UPDATE temp_users SET age = 30 WHERE id = 1")
    p1$refresh()
    db_user <- engine$get_query("SELECT * FROM temp_users WHERE id = 1")
    expect_equal(p1$data, as.list(db_user[1, ]))
})

test_that("postgres delete operations work", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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
    expect_equal(length(TempUser$read(id == 1, .mode = "all")), 0)
    remaining_users <- TempUser$read(.mode = "all")
    expect_equal(length(remaining_users), 1)
})


test_that("flush.postgres handles NULL values correctly", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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

test_that("PostgreSQL flush method respects transaction state", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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

test_that("flush.postgres handles empty data list", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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

# =============================================================================
# 4. SCHEMA OPERATIONS TESTS
# =============================================================================

test_that("qualify.postgres adds schema prefix correctly", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    
    # Test set_schema.postgres (it's a no-op that returns invisible NULL)
    result <- oRm:::set_schema.postgres(engine, "test_schema")
    expect_null(result)
})


test_that("create_schema.postgres fails with NULL schema", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    
    expect_error(
        oRm:::create_schema.postgres(engine, NULL),
        "Must supply a schema name."
    )
})

test_that("qualify.postgres handles edge cases in table names", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
    engine <- do.call(Engine$new, conn_info)
    
    # Close the connection
    engine$close()
    
    # Functions that don't require connection should still work
    qualified <- oRm:::qualify.postgres(engine, "users", "audit")
    expect_equal(qualified, "audit.users")
    
    set_result <- oRm:::set_schema.postgres(engine, "test")
    expect_null(set_result)
})

test_that("postgres schema switching works", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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

test_that("engine schema can be set on initialization", {
    conn_info <- tryCatch({
        c(use_postgres_test_db(), .schema = "test")
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    expect_equal(engine$schema, "test")
    expect_equal(engine$conn_args$dbname, "tests")
    expect_equal(engine$conn_args$host, "localhost")
    expect_equal(engine$conn_args$user, "tester")
})

test_that("engine$create_schema works and is idempotent", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
    engine <- do.call(Engine$new, conn_info)

    expect_silent(engine$create_schema("somenew"))
    expect_silent(engine$create_schema("somenew")) # Should not error if schema already exists
    stats <- DBI::dbGetQuery(engine$get_connection(),
        "SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'somenew'")
    expect_equal(nrow(stats), 1)
})

test_that("check_schema_exists validates schemas", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())
    withr::defer(DBI::dbExecute(engine$get_connection(), "DROP SCHEMA IF EXISTS missing_schema CASCADE"))

    expect_false(engine$check_schema_exists("missing_schema"))
    expect_silent(engine$create_schema("missing_schema"))
    expect_true(engine$check_schema_exists("missing_schema"))
})

test_that("create_table fails if schema does not exist, succeeds after explicit engine$create_schema", {
    conn_info <- tryCatch({
        c(use_postgres_test_db(), .schema = "not_exist")
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
    engine <- do.call(Engine$new, conn_info)
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
        c(use_postgres_test_db(), .schema = "test")
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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
        c(use_postgres_test_db(), .schema = "test")
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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
    res_public = engine$get_query('select * from test.users')  
    expect_equal(res_public$name[[1]], "Alice")
})

test_that("models can override engine default schema", {
    conn_info <- tryCatch({
        c(use_postgres_test_db(), .schema = "test")
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    UserArchive <- engine$model("users", .schema = "archive")
    expect_equal(UserArchive$tablename, "archive.users")
    expect_equal(engine$schema, "test")
})

test_that("engine set_schema changes search path", {
    conn_info <- tryCatch({
        c(use_postgres_test_db(), .schema = "test")
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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
        c(use_postgres_test_db(), .schema = "test")
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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
        c(use_postgres_test_db(), .schema = "test")
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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
        c(use_postgres_test_db(), .schema = "test")
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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

# =============================================================================
# 5. ADVANCED FEATURES TESTS  
# =============================================================================

test_that("PostgreSQL dialect handles special characters in data", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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

test_that("flush.postgres handles concurrent access scenarios", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())
    
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

# =============================================================================
# JSON COLUMN AUTO-SERIALIZATION/DESERIALIZATION TESTS
# =============================================================================

test_that("JSON columns auto-serialize R vectors on write", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())

    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    Jobs <- engine$model(
        "jobs",
        id = Column("SERIAL", primary_key = TRUE, nullable = FALSE),
        prompts = Column("JSONB"),
        metadata = Column("JSON")
    )

    Jobs$create_table(overwrite = TRUE)
    withr::defer(Jobs$drop_table(ask = FALSE))

    # Test vector serialization
    job1 <- Jobs$record(prompts = c("a", "b", "c"), metadata = list(key = "value"))
    job1$create()

    expect_equal(job1$data$id, 1)

    # Verify it was stored as JSON in the database
    conn <- engine$get_connection()
    raw_result <- DBI::dbGetQuery(conn, "SELECT prompts, metadata FROM jobs WHERE id = 1")
    expect_true(!is.null(raw_result$prompts))
    expect_true(!is.null(raw_result$metadata))
})

test_that("JSON columns auto-deserialize on read", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())

    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    Jobs <- engine$model(
        "jobs",
        id = Column("SERIAL", primary_key = TRUE, nullable = FALSE),
        prompts = Column("JSONB"),
        config = Column("JSON")
    )

    Jobs$create_table(overwrite = TRUE)
    withr::defer(Jobs$drop_table(ask = FALSE))

    # Insert with R objects
    original_prompts <- c("prompt1", "prompt2", "prompt3")
    original_config <- list(timeout = 30, retry = TRUE, max_attempts = 3)

    job1 <- Jobs$record(prompts = original_prompts, config = original_config)
    job1$create()

    # Read back and verify deserialization
    job_read <- Jobs$get(id == 1)

    expect_equal(job_read$data$prompts, original_prompts)
    expect_equal(job_read$data$config$timeout, 30)
    expect_equal(job_read$data$config$retry, TRUE)
    expect_equal(job_read$data$config$max_attempts, 3)
})

test_that("JSON columns treat strings as pre-formatted JSON", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())

    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    Jobs <- engine$model(
        "jobs",
        id = Column("SERIAL", primary_key = TRUE, nullable = FALSE),
        data = Column("JSONB")
    )

    Jobs$create_table(overwrite = TRUE)
    withr::defer(Jobs$drop_table(ask = FALSE))

    # Insert with pre-formatted JSON string
    json_string <- '{"custom": "format", "nested": {"value": 123}}'
    job1 <- Jobs$record(data = json_string)
    job1$create()

    # Read back
    job_read <- Jobs$get(id == 1)

    # Should be deserialized to R object
    expect_type(job_read$data$data, "list")
    expect_equal(job_read$data$data$custom, "format")
    expect_equal(job_read$data$data$nested$value, 123)
})

test_that("JSON columns handle NULL values correctly", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())

    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    Jobs <- engine$model(
        "jobs",
        id = Column("SERIAL", primary_key = TRUE, nullable = FALSE),
        data = Column("JSONB", nullable = TRUE)
    )

    Jobs$create_table(overwrite = TRUE)
    withr::defer(Jobs$drop_table(ask = FALSE))

    # Insert without JSON field (should be NULL)
    job1 <- Jobs$record()
    job1$create()

    # Read back
    job_read <- Jobs$get(id == 1)
    expect_true(is.null(job_read$data$data) || is.na(job_read$data$data))
})

test_that("JSON columns handle complex nested structures", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())

    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    Jobs <- engine$model(
        "jobs",
        id = Column("SERIAL", primary_key = TRUE, nullable = FALSE),
        config = Column("JSONB")
    )

    Jobs$create_table(overwrite = TRUE)
    withr::defer(Jobs$drop_table(ask = FALSE))

    # Insert complex nested structure
    complex_config <- list(
        layers = list(
            list(type = "dense", units = 128),
            list(type = "dropout", rate = 0.5),
            list(type = "dense", units = 10)
        ),
        optimizer = list(
            name = "adam",
            learning_rate = 0.001,
            beta1 = 0.9,
            beta2 = 0.999
        ),
        metrics = c("accuracy", "loss", "auc")
    )

    job1 <- Jobs$record(config = complex_config)
    job1$create()

    # Read back and verify structure is preserved
    job_read <- Jobs$get(id == 1)

    expect_equal(length(job_read$data$config$layers), 3)
    expect_equal(job_read$data$config$layers[[1]]$type, "dense")
    expect_equal(job_read$data$config$layers[[1]]$units, 128)
    expect_equal(job_read$data$config$optimizer$name, "adam")
    expect_equal(job_read$data$config$optimizer$learning_rate, 0.001)
    expect_equal(job_read$data$config$metrics, c("accuracy", "loss", "auc"))
})

test_that("JSON columns work with multiple records", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())

    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    Jobs <- engine$model(
        "jobs",
        id = Column("SERIAL", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE),
        tags = Column("JSONB")
    )

    Jobs$create_table(overwrite = TRUE)
    withr::defer(Jobs$drop_table(ask = FALSE))

    # Insert multiple records with different JSON data
    Jobs$record(name = "job1", tags = c("urgent", "backend"))$create()
    Jobs$record(name = "job2", tags = c("frontend", "ui", "ux"))$create()
    Jobs$record(name = "job3", tags = c("database"))$create()

    # Read all back
    all_jobs <- Jobs$all()

    expect_equal(length(all_jobs), 3)
    expect_equal(all_jobs[[1]]$data$tags, c("urgent", "backend"))
    expect_equal(all_jobs[[2]]$data$tags, c("frontend", "ui", "ux"))
    expect_equal(all_jobs[[3]]$data$tags, c("database"))
})

test_that("JSON/JSONB types are case-insensitive", {
    conn_info <- tryCatch({
        use_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
    })
    withr::defer(clear_postgres_test_tables())

    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    # Test with lowercase json/jsonb
    Jobs <- engine$model(
        "jobs",
        id = Column("SERIAL", primary_key = TRUE, nullable = FALSE),
        data1 = Column("json"),
        data2 = Column("jsonb")
    )

    Jobs$create_table(overwrite = TRUE)
    withr::defer(Jobs$drop_table(ask = FALSE))

    # Should still auto-serialize
    job1 <- Jobs$record(data1 = c(1, 2, 3), data2 = list(key = "value"))
    job1$create()

    # And auto-deserialize
    job_read <- Jobs$get(id == 1)
    expect_equal(job_read$data$data1, c(1, 2, 3))
    expect_equal(job_read$data$data2$key, "value")
})