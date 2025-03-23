test_that("TableModel initializes and defines fields correctly", {

  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:"
  )

  model <- TableModel$new(
    tablename = "test_TableModel",
    engine = engine,
    id = Column("INTEGER", key = TRUE, nullable = FALSE),
    name = Column("TEXT", nullable = FALSE),
    created_at = Column("TIMESTAMP")
  )

  # Check table name and engine binding
  expect_equal(model$tablename, "test_TableModel")
  expect_identical(model$engine, engine)

  # Check that all expected fields are present
  expect_setequal(names(model$fields), c("id", "name", "created_at"))

  # Check field types
  dbi_fields <- model$get_fields_for_dbi()
  expect_type(dbi_fields, "character")
  expect_equal(dbi_fields[["id"]], "INTEGER")
  expect_equal(dbi_fields[["name"]], "TEXT")

  # Create the table in the DB
  con <- model$get_connection()
  expect_true(DBI::dbIsValid(con))

  DBI::dbExecute(con, "DROP TABLE IF EXISTS test_TableModel")
  model$create_table()
  expect_true("test_TableModel" %in% DBI::dbListTables(con))

  # Print shouldn't error
  # expect_no_error(invisible(model$print()))

  # Clean up
  DBI::dbExecute(con, "DROP TABLE IF EXISTS test_TableModel")
  engine$close()
})

test_that("TableModel$read() works with filter expressions and mode", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:"
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
    dbname = ":memory:"
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