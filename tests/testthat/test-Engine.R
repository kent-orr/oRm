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
  testthat::skip_if_not_installed("pool")
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

test_that("Engine stores schema and passes it to TableModel", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    .schema = "analytics"
  )

  expect_equal(engine$schema, "analytics")

  # SQLite ignores schema qualification and warns exactly once.
  expect_warning(
    model <- engine$model("users", id = Column("INTEGER", primary_key = TRUE)),
    "does not support schema qualification"
  )

  expect_s3_class(model, "TableModel")
  expect_equal(model$schema, "analytics")
  # For this test, we don't care about the exact tablename format - that's dialect-specific
  expect_true(grepl("users", model$tablename))

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

  engine$close()
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
  
  # Test: with.Engine warns about the rollback, then re-raises the error.
  expect_warning(
    expect_error(
      with.Engine(engine, {
        model$record(id = 1, name = "Alice")$create()
        model$record(id = 2, name = NULL)$create()  # This should fail
      })
    ),
    "Transaction failed, rolling back"
  )
  
  # Verify that no records were inserted due to rollback
  all_records <- model$read()
  expect_equal(length(all_records), 0)

  engine$close()
})

test_that("with.Engine maintains transaction state correctly", {
  # Setup
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )
  
  # Test: the forced error triggers a rollback warning before propagating.
  suppressWarnings(
    tryCatch(
      with.Engine(engine, {
        expect_true(engine$get_transaction_state())
        stop("Forced error")
      }),
      error = function(e) {}
    )
  )
  
  expect_false(engine$get_transaction_state())
  
  with.Engine(engine, {
    expect_true(engine$get_transaction_state())
  })

  expect_false(engine$get_transaction_state())

  engine$close()
})

test_that("dialect detected when driver passed as unnamed positional argument", {
  engine <- Engine$new(
    RSQLite::SQLite(),
    dbname = ":memory:"
  )
  expect_equal(engine$dialect, "sqlite")
  expect_equal(engine$conn_args[["drv"]], RSQLite::SQLite())
})


test_that("autoflush populates server-generated values inside a transaction", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )
  withr::defer(engine$close())

  model <- engine$model(
    "autoflush_table",
    id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
    name = Column("TEXT")
  )
  model$create_table()

  # Default: a create inside a transaction is a plain insert, so the generated
  # key never comes back on the record (the behaviour reported in #122).
  plain <- with.Engine(engine, {
    model$record(name = "Alice")$create()
  })
  expect_null(plain$data$id)

  # .autoflush = TRUE makes the same create return the generated key.
  flushed <- with.Engine(engine, {
    model$record(name = "Bob")$create()
  }, .autoflush = TRUE)
  expect_false(is.null(flushed$data$id))
  expect_equal(flushed$data$name, "Bob")

  # Both rows committed either way.
  expect_equal(length(model$read()), 2)
})

test_that("autoflush respects explicit flush_record and restores prior state", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )
  withr::defer(engine$close())

  model <- engine$model(
    "autoflush_optout",
    id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
    name = Column("TEXT")
  )
  model$create_table()

  # An explicit flush_record still wins over the block setting.
  opted_out <- with.Engine(engine, {
    model$record(name = "Alice")$create(flush_record = FALSE)
  }, .autoflush = TRUE)
  expect_null(opted_out$data$id)

  # The setting is scoped to the block.
  expect_false(engine$get_autoflush())

  # Including when the block errors.
  suppressWarnings(try(
    with.Engine(engine, {
      stop("Forced error")
    }, .autoflush = TRUE),
    silent = TRUE
  ))
  expect_false(engine$get_autoflush())

  # Outside a transaction creates flush regardless of the setting.
  outside <- model$record(name = "Carol")$create()
  expect_false(is.null(outside$data$id))
})

test_that("nested with.Engine blocks inherit and scope autoflush", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )
  withr::defer(engine$close())

  model <- engine$model(
    "autoflush_nested",
    id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
    name = Column("TEXT")
  )
  model$create_table()

  with.Engine(engine, {
    # An inner block that says nothing keeps the outer setting.
    inner <- with.Engine(engine, {
      expect_true(engine$get_autoflush())
      model$record(name = "inner")$create()
    })
    expect_false(is.null(inner$data$id))

    # An inner block may turn it off for its own scope only.
    with.Engine(engine, {
      expect_false(engine$get_autoflush())
    }, .autoflush = FALSE)
    expect_true(engine$get_autoflush())
  }, .autoflush = TRUE)

  expect_false(engine$get_autoflush())
})

test_that("with.Engine rejects a non-logical .autoflush", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )
  withr::defer(engine$close())

  expect_error(
    with.Engine(engine, invisible(NULL), .autoflush = "yes"),
    "must be TRUE, FALSE, or NULL"
  )
  # The rejected call must not have opened a transaction.
  expect_false(engine$get_transaction_state())
})

test_that("Engine(.autoflush = TRUE) sets the in-transaction flush default", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE,
    .autoflush = TRUE
  )
  withr::defer(engine$close())

  model <- engine$model(
    "engine_autoflush",
    id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
    name = Column("TEXT")
  )
  model$create_table()

  expect_true(engine$get_autoflush())

  # No per-block flag needed: creates inside a transaction come back populated.
  parent <- with.Engine(engine, {
    model$record(name = "Alice")$create()
  })
  expect_false(is.null(parent$data$id))

  # A block may still turn it off, and the engine default is restored after.
  off <- with.Engine(engine, {
    expect_false(engine$get_autoflush())
    model$record(name = "Bob")$create()
  }, .autoflush = FALSE)
  expect_null(off$data$id)
  expect_true(engine$get_autoflush())

  # So may an individual create.
  opted_out <- with.Engine(engine, {
    model$record(name = "Carol")$create(flush_record = FALSE)
  })
  expect_null(opted_out$data$id)

  # set_autoflush() changes the default for the rest of the session and
  # reports the value it replaced.
  expect_true(engine$set_autoflush(FALSE))
  expect_false(engine$get_autoflush())
  plain <- with.Engine(engine, model$record(name = "Dave")$create())
  expect_null(plain$data$id)

  expect_equal(length(model$read()), 4)
})

test_that("Engine defaults to .autoflush = FALSE and validates the argument", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )
  withr::defer(engine$close())
  expect_false(engine$get_autoflush())

  expect_error(
    Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", .autoflush = "yes"),
    "must be TRUE or FALSE"
  )
  expect_error(
    Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", .autoflush = NA),
    "must be TRUE or FALSE"
  )
})
