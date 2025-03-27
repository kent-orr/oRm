test_that("TableModel initializes and defines fields correctly", {

  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )

  model <- TableModel$new(
    tablename = "test_TableModel",
    engine = engine,
    id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
    name = Column("TEXT", nullable = FALSE),
    created_at = Column("TIMESTAMP")
  )

  # Check table name and engine binding
  expect_equal(model$tablename, "test_TableModel")
  expect_identical(model$engine, engine)

  # Check that all expected fields are present
  expect_setequal(names(model$fields), c("id", "name", "created_at"))

  # Check field SQL definitions
  sql_fields <- model$generate_sql_fields()
  expect_type(sql_fields, "list")
  expect_equal(length(sql_fields), 3)

  # Check individual field definitions
  expect_true(grepl("`id` INTEGER .* PRIMARY KEY", sql_fields[[1]]))
  expect_true(grepl("`name` TEXT NOT NULL", sql_fields[[2]]))
  expect_true(grepl("`created_at` TIMESTAMP", sql_fields[[3]]))
  # Create the table in the DB
  con <- model$get_connection()
  expect_true(DBI::dbIsValid(con))

  DBI::dbExecute(con, "DROP TABLE IF EXISTS test_TableModel")
  model$create_table()
  expect_true("test_TableModel" %in% DBI::dbListTables(con))

  # Verify table structure
  table_info <- DBI::dbGetQuery(con, "PRAGMA table_info(test_TableModel)")
  expect_equal(nrow(table_info), 3)
  expect_equal(table_info$name, c("id", "name", "created_at"))
  expect_equal(table_info$type, c("INTEGER", "TEXT", "TIMESTAMP"))
  expect_equal(table_info$notnull, c(1, 1, 0))
  expect_equal(table_info$pk, c(1, 0, 0))
  # Print shouldn't error
  expect_no_error(print(model))

  # Clean up
  DBI::dbExecute(con, "DROP TABLE IF EXISTS test_TableModel")
  engine$close()
})


test_that("TableModel$read() works with filter expressions and mode", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )

  User <- engine$model(
    "users",
    id = Column("INTEGER", key = TRUE, nullable = FALSE),
    name = Column("TEXT", nullable = FALSE),
    age = Column("INTEGER")
  )

  User$create_table()

  # Insert multiple users
  Record$new(User, id = 1, name = "Alice", age = 30)$create()
  Record$new(User, id = 2, name = "Bob", age = 25)$create()
  Record$new(User, id = 3, name = "Charlie", age = 17)$create()

  # one_or_none: should return one Record
  rec <- Record$new(User)
  result <- rec$read(id == 1, mode = "one_or_none")

  expect_true(inherits(result, "Record"))
  expect_equal(result$data$name, "Alice")

  # all: should return list of Record objects
  teens <- rec$read(age >= 10, age < 20, mode = "all")
  expect_type(teens, "list")
  expect_true(all(vapply(teens, inherits, logical(1), "Record")))
  expect_equal(length(teens), 1)
  expect_equal(teens[[1]]$data$name, "Charlie")

  # get: fails if multiple rows match
  expect_error(
    rec$read(age > 18, mode = "get"),
    "Expected exactly one row"
  )

  # one_or_none: returns NULL if no match
  none <- rec$read(name == "Nobody", mode = "one_or_none")
  expect_null(none)

  engine$close()
})

test_that("TableModel$delete_where() deletes rows using filter expressions", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )

  User <- engine$model(
    "users",
    id = Column("INTEGER", key = TRUE, nullable = FALSE),
    name = Column("TEXT", nullable = FALSE),
    age = Column("INTEGER")
  )

  User$create_table()

  # Create three users
  Record$new(User, id = 1, name = "Alice", age = 30)$create()
  Record$new(User, id = 2, name = "Bob", age = 17)$create()
  Record$new(User, id = 3, name = "Charlie", age = 25)$create()

  con <- User$get_connection()

  # Confirm all rows exist
  initial <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM users")$n
  expect_equal(initial, 3)

  # Delete users under 18
  User$delete_where(age < 18)

  after_delete <- DBI::dbGetQuery(con, "SELECT * FROM users")
  expect_equal(nrow(after_delete), 2)
  expect_false(any(after_delete$name == "Bob"))

  # Delete remaining users
  User$delete_where(age >= 18)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM users")$n,
    0
  )

  engine$close()
})