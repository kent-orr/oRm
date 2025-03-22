test_that("Record$read() works with filter expressions and mode", {
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
  Record$new(User, list(id = 1, name = "Alice", age = 30))$create()
  Record$new(User, list(id = 2, name = "Bob", age = 25))$create()
  Record$new(User, list(id = 3, name = "Charlie", age = 17))$create()

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
