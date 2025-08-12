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

test_that("engine models create a schema if it does not exist", {
    conn_info <- tryCatch({
        c(setup_postgres_test_db(), .schema = "test")
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    # Debug: Check what schema the engine thinks it's using
    cat("Engine schema:", engine$schema, "\n")
    
    model <- engine$model("users", id = Column("SERIAL", primary_key = TRUE), name = Column("TEXT", nullable = FALSE))
    
    # Debug: Check what tablename the model has
    cat("Model tablename:", model$tablename, "\n")
    
    # Let's manually execute the SQL and see what happens
    conn <- engine$get_connection()
    
    # First, let's check if the schema exists
    schema_check <- DBI::dbGetQuery(conn, "SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'test'")
    cat("Schema 'test' exists:", nrow(schema_check) > 0, "\n")
    
    # Let's try creating the table manually step by step
    cat("Creating schema manually...\n")
    tryCatch({
        DBI::dbExecute(conn, "CREATE SCHEMA IF NOT EXISTS test")
        cat("Schema creation successful\n")
    }, error = function(e) cat("Schema creation failed:", e$message, "\n"))
    
    cat("Creating table manually...\n")
    tryCatch({
        result <- DBI::dbExecute(conn, 'CREATE TABLE IF NOT EXISTS "test"."users" (
            "id" SERIAL PRIMARY KEY,
            "name" TEXT NOT NULL
        )')
        cat("Table creation result:", result, "\n")
    }, error = function(e) cat("Table creation failed:", e$message, "\n"))
    
    # Now check if the table exists
    all_schema_tables <- DBI::dbGetQuery(conn, "
        SELECT schemaname, tablename 
        FROM pg_tables 
        WHERE tablename = 'users'
    ")
    cat("Tables named 'users' after manual creation:\n")
    print(all_schema_tables)
    
    # Check if we can now access the table
    tryCatch({
        result <- DBI::dbGetQuery(conn, 'SELECT * FROM "test"."users" LIMIT 0')
        cat("Manual table access works!\n")
        print(str(result))
    }, error = function(e) cat("Manual table access failed:", e$message, "\n"))
    
    # Now let's see what the model's create_table method actually does
    cat("Now trying model$create_table()...\n")
    sql_output <- model$create_table(overwrite = TRUE, verbose = TRUE)
    cat("SQL executed by model:", sql_output, "\n")
    
    # Check again after model creation
    all_schema_tables2 <- DBI::dbGetQuery(conn, "
        SELECT schemaname, tablename 
        FROM pg_tables 
        WHERE tablename = 'users'
    ")
    cat("Tables named 'users' after model creation:\n")
    print(all_schema_tables2)
})


test_that("connection persistence issue debugging", {
    conn_info <- tryCatch({
        c(setup_postgres_test_db(), .schema = "test")
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })
    withr::defer(cleanup_postgres_test_db())
    engine <- do.call(Engine$new, conn_info)
    withr::defer(engine$close())

    model <- engine$model("users", id = Column("SERIAL", primary_key = TRUE), name = Column("TEXT", nullable = FALSE))
    
    # Create table
    cat("Creating table...\n")
    model$create_table(overwrite = TRUE)
    
    # Immediately check if table exists using the same connection
    cat("Checking table existence immediately after creation...\n")
    conn1 <- engine$get_connection()
    tables1 <- DBI::dbGetQuery(conn1, "SELECT schemaname, tablename FROM pg_tables WHERE tablename = 'users'")
    cat("Tables found with conn1:\n")
    print(tables1)
    
    # Try to access the table immediately
    cat("Trying to access table with conn1...\n")
    tryCatch({
        result1 <- DBI::dbGetQuery(conn1, 'SELECT * FROM "test"."users" LIMIT 0')
        cat("Access with conn1 works!\n")
    }, error = function(e) cat("Access with conn1 failed:", e$message, "\n"))
    
    # Now get a fresh connection and try again
    cat("Getting fresh connection...\n")
    engine$close()  # Force close current connection
    conn2 <- engine$get_connection()
    
    cat("Checking table existence with fresh connection...\n")
    tables2 <- DBI::dbGetQuery(conn2, "SELECT schemaname, tablename FROM pg_tables WHERE tablename = 'users'")
    cat("Tables found with conn2:\n")
    print(tables2)
    
    # Try to access the table with fresh connection
    cat("Trying to access table with conn2...\n")
    tryCatch({
        result2 <- DBI::dbGetQuery(conn2, 'SELECT * FROM "test"."users" LIMIT 0')
        cat("Access with conn2 works!\n")
    }, error = function(e) cat("Access with conn2 failed:", e$message, "\n"))
    
    # Now try the model's read method
    cat("Trying model$read()...\n")
    tryCatch({
        records <- model$read(mode = "all")
        cat("Model read works! Found", length(records), "records\n")
    }, error = function(e) cat("Model read failed:", e$message, "\n"))
    
    # Check what connection the model is using
    cat("Checking model's connection...\n")
    conn3 <- model$get_connection()
    search_path <- DBI::dbGetQuery(conn3, "SHOW search_path")
    cat("Model's connection search_path:", search_path[[1]], "\n")
    
    # Try direct query through model's connection
    cat("Trying direct query through model's connection...\n")
    tryCatch({
        result3 <- DBI::dbGetQuery(conn3, 'SELECT * FROM "test"."users" LIMIT 0')
        cat("Direct query through model connection works!\n")
    }, error = function(e) cat("Direct query through model connection failed:", e$message, "\n"))
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

