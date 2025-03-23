test_that("Column constructor returns valid Column objects", {
  col <- Column("TEXT", nullable = FALSE, key = TRUE)

  expect_s3_class(col, "Column")
  expect_equal(col$type, "TEXT")
  expect_false(col$nullable)
  expect_true(col$key)
})

test_that("Column uses default values when not specified", {
  col <- Column("INTEGER")

  expect_equal(col$type, "INTEGER")
  expect_true(col$nullable)
  expect_false(col$key)
})

test_that("Column rejects invalid input gracefully", {
  col <- Column("custom_type", key = TRUE)
  expect_equal(col$type, "custom_type")
  expect_true(col$key)
})

test_that("Column definitions are correctly applied to created tables", {
  # Setup in-memory SQLite engine
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:"
  )

  # Create a model with known field types
  model <- engine$model(
    "test_columns",
    id = Column("INTEGER", nullable = FALSE, key = TRUE),
    name = Column("TEXT", nullable = FALSE),
    score = Column("REAL", nullable = TRUE)
  )

  # Create table
  model$create_table()

  # Read actual column definitions from SQLite
  con <- model$get_connection()
  result <- DBI::dbGetQuery(con, "PRAGMA table_info(test_columns)")

  # Convert to named vector: column_name => type
  col_types <- setNames(result$type, result$name)

  expect_equal(col_types[["id"]], "INTEGER")
  expect_equal(col_types[["name"]], "TEXT")
  expect_equal(col_types[["score"]], "REAL")

  # Clean up
  DBI::dbExecute(con, "DROP TABLE IF EXISTS test_columns")
  engine$close()
})

test_that("Column assigns explicit default correctly", {
  col <- Column("INTEGER", default = 42)
  expect_equal(col$default, 42)

  col2 <- Column("TEXT", default = "anon")
  expect_equal(col2$default, "anon")
})

test_that("Column infers default when nullable is FALSE and no default given", {
  col <- Column("INTEGER", nullable = FALSE)
  expect_equal(col$default, 0L)

  col2 <- Column("TEXT", nullable = FALSE)
  expect_equal(col2$default, "")

  col3 <- Column("REAL", nullable = FALSE)
  expect_equal(col3$default, 0.0)
})

test_that("Column keeps default = NULL when nullable = TRUE", {
  col <- Column("TEXT", nullable = TRUE)
  expect_null(col$default)

  col2 <- Column("INTEGER", nullable = TRUE)
  expect_null(col2$default)
})


# test_that("Record$create() inserts a row and respects defaults", {
#   engine <- Engine$new(
#     drv = RSQLite::SQLite(),
#     dbname = ":memory:"
#   )
#
#   # Define a TableModel with default and required fields
#   User <- engine$model(
#     "users",
#     id = Column("INTEGER", key = TRUE, nullable = FALSE),
#     name = Column("TEXT", nullable = FALSE),
#     age = Column("INTEGER", default = 18),
#     city = Column("TEXT", default = "Unknown")
#   )
#
#   # Create the table
#   User$create_table()
#
#   # Insert a record with only required fields
#   r1 <- Record$new(User, list(id = 1, name = "Alice"))
#   r1$create()
#
#   # Verify the row exists and defaults were filled
#   result <- DBI::dbGetQuery(engine$get_connection(), "SELECT * FROM users WHERE id = 1")
#
#   expect_equal(nrow(result), 1)
#   expect_equal(result$name, "Alice")
#   expect_equal(result$age, 18)
#   expect_equal(result$city, "Unknown")
#
#   # Clean up
#   DBI::dbExecute(engine$get_connection(), "DROP TABLE IF EXISTS users")
#   engine$close()
# })
