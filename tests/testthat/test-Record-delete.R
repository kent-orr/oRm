test_that("Record$delete() removes the row from the database", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:"
  )

  # Define a basic user model
  User <- engine$model(
    "users",
    id = Column("INTEGER", key = TRUE, nullable = FALSE),
    name = Column("TEXT", nullable = FALSE),
    age = Column("INTEGER")
  )

  User$create_table()

  # Insert a record
  user <- Record$new(User, id = 1, name = "Alice", age = 30)
  user$create()

  # Confirm it exists
  con <- engine$get_connection()
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM users")$n,
    1
  )

  # Delete it
  user$delete()

  # Confirm it's gone
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM users")$n,
    0
  )

  engine$close()
})
