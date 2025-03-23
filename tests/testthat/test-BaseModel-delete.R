test_that("BaseModel$delete_where() deletes rows using filter expressions", {
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
  Record$new(User, list(id = 1, name = "Alice", age = 30))$create()
  Record$new(User, list(id = 2, name = "Bob", age = 17))$create()
  Record$new(User, list(id = 3, name = "Charlie", age = 25))$create()

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
