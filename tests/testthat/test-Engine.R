test_that("Engine connects, runs queries, and executes SQL", {
 
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )


  # Ensure connection is valid
  con <- engine$get_connection()
  expect_true(DBI::dbIsValid(con))
  expect_true(inherits(con, "DBIConnection"))

  # list_tables() should return a character vector (empty or not)
  tables <- engine$list_tables()
  expect_type(tables, "character")

  # get_query() should return a data.frame
  result <- engine$get_query("SELECT 1 AS test_col")
  expect_s3_class(result, "data.frame")
  expect_equal(result$test_col, 1)

  # execute() should work — we'll create and drop a temp table
  engine$execute("CREATE TABLE test_temp (id INT)")
  tables_after_create <- engine$list_tables()
  expect_true(any(grepl("test_temp", tables_after_create)))

  engine$execute("DROP TABLE IF EXISTS test_temp")
  tables_after_drop <- engine$list_tables()
  expect_false("test_temp" %in% tables_after_drop)

  # Close and confirm the connection is invalid
  engine$close()
  expect_false(DBI::dbIsValid(con))
})

test_that("Engine creates a connection pool when use_pool is TRUE", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    use_pool = TRUE
  )
  
  # Check that the connection is a pool object
  con <- engine$get_connection()
  expect_s3_class(con, "Pool")
  
  # Verify that the pool is working by executing a query
  result <- engine$get_query("SELECT 1 AS test_col")
  expect_s3_class(result, "data.frame")
  expect_equal(result$test_col, 1)
  
  # Check that closing the engine closes the pool
  engine$close()
  expect_null(engine$conn)
  
  # Attempt to get a new connection should create a new pool
  new_con <- engine$get_connection()
  expect_s3_class(new_con, "Pool")
  expect_false(identical(con, new_con))
  
  # Clean up
  engine$close()
})


test_that("Engine maintains an open connection when persist is set to TRUE", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )
  
  # Get initial connection
  initial_conn <- engine$get_connection()
  expect_true(DBI::dbIsValid(initial_conn))
  
  # Perform some operations
  engine$execute("CREATE TABLE test_persist (id INTEGER)")
  engine$get_query("SELECT * FROM test_persist")
  engine$list_tables()
  
  # Check if the connection is still valid after operations
  expect_true(DBI::dbIsValid(initial_conn))
  
  # Get connection again and check if it's the same
  second_conn <- engine$get_connection()
  expect_identical(initial_conn, second_conn)
  
  # Close the connection manually
  engine$close()
  expect_false(DBI::dbIsValid(initial_conn))
})

test_that("Engine closes the connection after operations when persist is FALSE", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = FALSE
  )
  
  # Perform an operation
  result <- engine$get_query("SELECT 1 AS test_col")
  expect_s3_class(result, "data.frame")
  expect_equal(result$test_col, 1)
  
  # Check if the connection is closed after the operation
  expect_null(engine$conn)
  
  # Try to get a new connection
  new_conn <- engine$get_connection()
  expect_true(DBI::dbIsValid(new_conn))
  
  # Perform another operation
  tables <- engine$list_tables()
  expect_type(tables, "character")
  
  # Check if the connection is closed again
  expect_null(engine$conn)
  
  # Clean up
  engine$close()
})

test_that("Engine handles multiple calls to close() without errors", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )
  
  # Get initial connection
  initial_conn <- engine$get_connection()
  expect_true(DBI::dbIsValid(initial_conn))
  
  # First close
  expect_no_error(engine$close())
  expect_false(DBI::dbIsValid(initial_conn))
  expect_null(engine$conn)
  
  # Second close (should not raise an error)
  expect_no_error(engine$close())
  expect_null(engine$conn)
  
  # Third close (should still not raise an error)
  expect_no_error(engine$close())
  expect_null(engine$conn)
  
  # Get a new connection after multiple closes
  new_conn <- engine$get_connection()
  expect_true(DBI::dbIsValid(new_conn))
  
  # Clean up
  engine$close()
})

test_that("Engine throws an error when trying to get a connection with invalid credentials", {
  # Create an Engine object with invalid credentials
  invalid_engine <- Engine$new(
    drv = "drive"
  )
  
  # Attempt to get a connection and expect an error
  expect_error(
    invalid_engine$get_connection()
  )
  
  # Ensure the connection is still NULL after the failed attempt
  expect_null(invalid_engine$conn)
})

test_that("Engine correctly handles conn_args passed as a list in the constructor", {
  # Create a list of connection arguments
  conn_args <- list(
    drv = RSQLite::SQLite(),
    dbname = ":memory:"
  )
  
  # Create an Engine object with conn_args
  engine <- Engine$new(conn_args = conn_args)
  
  # Check that conn_args are correctly set
  expect_equal(engine$conn_args, conn_args)
  
  # Verify that a connection can be established
  con <- engine$get_connection()
  expect_true(DBI::dbIsValid(con))
  
  # Verify that the connection is using the correct driver and database
  expect_s4_class(con, "SQLiteConnection")
  expect_equal(con@dbname, ":memory:")
  
  # Clean up
  engine$close()
})

test_that("Engine constructor combines individual arguments and conn_args list, prioritizing individual arguments", {
  # Create a conn_args list
  conn_args <- list(
    drv = RSQLite::SQLite(),
    dbname = "test_db.sqlite",
    schema = "public"
  )
  
  # Create an Engine object with both individual arguments and conn_args
  engine <- Engine$new(
    dbname = ":memory:",
    conn_args = conn_args
  )
  
  # Check that the arguments were combined, with individual arguments prioritized
  expect_equal(engine$conn_args$drv, RSQLite::SQLite())
  expect_equal(engine$conn_args$dbname, ":memory:")  # Prioritized over conn_args
  expect_equal(engine$conn_args$schema, "public")    # Retained from conn_args
  
  # Verify that a connection can be established with the combined arguments
  con <- engine$get_connection()
  expect_true(DBI::dbIsValid(con))
  expect_s4_class(con, "SQLiteConnection")
  expect_equal(con@dbname, ":memory:")
  
  # Clean up
  engine$close()
})

test_that("Engine returns the same connection object on multiple calls to get_connection()", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )
  
  # Get the first connection
  conn1 <- engine$get_connection()
  expect_true(DBI::dbIsValid(conn1))
  
  # Get the second connection
  conn2 <- engine$get_connection()
  expect_true(DBI::dbIsValid(conn2))
  
  # Check if both connections are the same object
  expect_identical(conn1, conn2)
  
  # Clean up
  engine$close()
})

test_that("Engine creates a new connection when get_connection() is called after close()", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )
  
  # Get initial connection
  initial_conn <- engine$get_connection()
  expect_true(DBI::dbIsValid(initial_conn))
  
  # Close the connection
  engine$close()
  expect_false(DBI::dbIsValid(initial_conn))
  expect_null(engine$conn)
  
  # Get a new connection
  new_conn <- engine$get_connection()
  expect_true(DBI::dbIsValid(new_conn))
  expect_false(identical(initial_conn, new_conn))
  
  # Clean up
  engine$close()
})

test_that("Engine handles errors gracefully when executing invalid SQL queries", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )
  
  # Test invalid SQL in get_query
  expect_error(
    engine$get_query("SELECT * FROM nonexistent_table"),
    "no such table: nonexistent_table"
  )
  
  # Test invalid SQL in execute
  expect_error(
    engine$execute("INSERT INTO nonexistent_table (column) VALUES (1)"),
    "no such table: nonexistent_table"
  )
  
  # Ensure the connection is still valid after errors
  expect_true(DBI::dbIsValid(engine$get_connection()))
  
  # Clean up
  engine$close()
})

test_that("Engine can generate a TableModel using model() method", {
  
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )

  user_model <- engine$model("users")

  expect_s3_class(user_model, "TableModel")
  expect_equal(user_model$tablename, "users")
  expect_identical(user_model$engine, engine)

  con <- user_model$get_connection()
  expect_true(DBI::dbIsValid(con))

  engine$close()
  expect_false(DBI::dbIsValid(con))
})


test_that("with.Engine executes successful transactions", {
  # Setup
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )
  
  model <- engine$model(
    "test_table",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT")
  )
  
  model$create_table()
  
  # Test
  result <- with.Engine(engine, {
    record <- model$record(id = 1, name = "Alice")$create()
    record$data
  })
  
  # Assertions
  expect_type(result, "list")
  expect_equal(result$id, 1)
  expect_equal(result$name, "Alice")
  
  # Verify the record was actually inserted
  saved_record <- model$read(id == 1)
  expect_equal(saved_record[[1]]$data$name, "Alice")
})

test_that("with.Engine rolls back failed transactions", {
  # Setup
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )
  
  model <- engine$model(
    "test_table",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT", nullable = FALSE)
  )
  
  model$create_table()
  
  # Test
  expect_error(
    with.Engine(engine, {
      model$record(id = 1, name = "Alice")$create()
      model$record(id = 2, name = NULL)$create()  # This should fail
    })
  )
  
  # Verify that no records were inserted due to rollback
  all_records <- model$read()
  expect_equal(length(all_records), 0)
})

test_that("with.Engine maintains transaction state correctly", {
  # Setup
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )
  
  # Test
  tryCatch(
    with.Engine(engine, {
      expect_true(engine$get_transaction_state())
      stop("Forced error")
    }),
    error = function(e) {}
  )
  
  expect_false(engine$get_transaction_state())
  
  with.Engine(engine, {
    expect_true(engine$get_transaction_state())
  })
  
  expect_false(engine$get_transaction_state())
})

