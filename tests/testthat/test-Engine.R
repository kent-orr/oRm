test_that("Engine connects, runs queries, and executes SQL", {
 
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:"
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
  engine$execute("CREATE TEMP TABLE test_temp (id INT)")
  tables_after_create <- engine$list_tables()
  expect_true(any(grepl("test_temp", tables_after_create)))

  engine$execute("DROP TABLE IF EXISTS test_temp")
  tables_after_drop <- engine$list_tables()
  expect_false("test_temp" %in% tables_after_drop)

  # Close and confirm the connection is invalid
  engine$close()
  expect_false(DBI::dbIsValid(con))
})


test_that("Engine can generate a BaseModel using model() method", {
  
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:"
  )

  user_model <- engine$model("users")

  expect_s3_class(user_model, "BaseModel")
  expect_equal(user_model$tablename, "users")
  expect_identical(user_model$engine, engine)

  con <- user_model$get_connection()
  expect_true(DBI::dbIsValid(con))

  engine$close()
  expect_false(DBI::dbIsValid(con))
})


test_that("Engine can generate a BaseModel using model() method", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:"
  )

  user_model <- engine$model("users")

  expect_true(inherits(user_model, "BaseModel"))
  expect_equal(user_model$tablename, "users")
  expect_identical(user_model$engine, engine)

  con <- user_model$get_connection()
  expect_true(DBI::dbIsValid(con))

  engine$close()
  expect_false(DBI::dbIsValid(con))
})
