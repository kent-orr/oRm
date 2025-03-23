test_that("Record$update() modifies a row in the database", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:"
  )

  # Define a model with a key and a few fields
  User <- engine$model(
    "users",
    id = Column("INTEGER", key = TRUE, nullable = FALSE),
    name = Column("TEXT", nullable = FALSE),
    age = Column("INTEGER", default = 0)
  )

  User$create_table()

  # Create and insert initial record
  rec <- Record$new(User, list(id = 1, name = "Alice", age = 25))
  rec$create()

  # Modify the record's fields
  rec$data$name <- "Alicia"
  rec$data$age <- 30
  rec$update()

  # Verify the changes were saved
  result <- DBI::dbGetQuery(engine$get_connection(), "SELECT * FROM users WHERE id = 1")

  expect_equal(nrow(result), 1)
  expect_equal(result$name, "Alicia")
  expect_equal(result$age, 30)

  engine$close()
})
