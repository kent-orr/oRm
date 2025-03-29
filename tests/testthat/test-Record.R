test_that("Record$create() inserts a row into the database", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:"
  )

  User <- engine$model(
    "users",
    id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
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
  expect_equal(result$age, 99)

  engine$close()
})


test_that("Relationships work correctly", {
  # Setup
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )

  User <- engine$model("users",
    id = Column("INTEGER", primary_key = TRUE),
    organization_id = ForeignKey("INTEGER", references = "organizations.id")
  )

  Organization <- engine$model("organizations",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("VARCHAR", nullable = FALSE)
  )

  Organization$create_table(overwrite = TRUE)
  User$create_table(overwrite = TRUE)
  # Create tables
  expect_no_error(Organization$create_table(overwrite = TRUE))
  expect_no_error(User$create_table(overwrite = TRUE))

  # Insert data
  org <- Organization$record(id = 100, name = "Data Corp")
  #org$create()
  expect_no_error(org$create())

  Organization |>
    define_relationship(User, 'one_to_one', User, 'id', 'organization_id')

  user <- User$record(id = 1, organization_id = 100)
#  user$create()

  expect_no_error(user$create())

  # Retrieve and test user data
  u <- User$read(id == 1, mode = "get")
  expect_equal(u$data$id, 1)
  expect_equal(u$data$organization_id, 100)

  # Test relationship
  related_org <- u$relationship('organization')
  expect_s3_class(related_org, "Record")
  expect_equal(related_org$data$id, 100)
  expect_equal(related_org$data$name, "Data Corp")

  # Clean up
  engine$close()
})