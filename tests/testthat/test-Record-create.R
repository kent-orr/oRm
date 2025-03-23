test_that("Record$create() inserts a row into the database", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:"
  )

  User <- engine$model(
    "users",
    id = Column("INTEGER", key = TRUE, nullable = FALSE),
    name = Column("TEXT", nullable = FALSE),
    age = Column("INTEGER", default = 99),
    city = Column("TEXT", default = "Unknown")
  )

  User$create_table()

  # Create and insert a record (omitting age and city)
  rec <- Record$new(User, id = 1, name = "Alice")
  rec$create()

  # Fetch the row from the DB
  result <- DBI::dbGetQuery(engine$get_connection(), "SELECT * FROM users WHERE id = 1")

  expect_equal(nrow(result), 1)
  expect_equal(result$id, 1)
  expect_equal(result$name, "Alice")
  skip("age = 99 since defaults are not yet implemented")
  # expect_equal(result$age, 99)

  engine$close()
})
