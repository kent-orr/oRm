test_that("The desired syntax can be used to create and work with models", {

  engine = Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:"
  )

  User = engine$model(
    "users",
    id = Column("INTEGER", key = TRUE, nullable = FALSE),
    name = Column("TEXT", nullable = FALSE),
    age = Column("INTEGER")
  )

  User$create_table()

  # Create
  alice = User$record(id = 1, name = "Alice", age = 30)
  res = DBI::dbGetQuery(engine$get_connection(), 'select * from users where id = 1')
  expect_equal(
    res, 
    data.frame(id = 1, name = "Alice", age = 30),
    info = "User creation failed: The created user does not match the expected data."
  )

  # Read
  read_alice = User$get(id = 1)
  expect_equal(
    read_alice$to_list(), 
    list(id = 1, name = "Alice", age = 30),
    info = "User read failed: The retrieved user data does not match the expected values."
  )

  # Update
  alice$age = 31
  alice$update()

  res = DBI::dbGetQuery(engine$get_connection(), 'select * from users where id = 1')
  expect_equal(
    res, 
    data.frame(id = 1, name = "Alice", age = 31),
    info = "User update (method 1) failed: The updated age is not reflected in the database."
  )

  alice$update(age = 32)

  res = DBI::dbGetQuery(engine$get_connection(), 'select * from users where id = 1')
  expect_equal(
    res, 
    data.frame(id = 1, name = "Alice", age = 32),
    info = "User update (method 2) failed: The updated age is not reflected in the database."
  )

  alice$update(.data=list(age = 33))

  res = DBI::dbGetQuery(engine$get_connection(), 'select * from users where id = 1')
  expect_equal(
    res, 
    data.frame(id = 1, name = "Alice", age = 33),
    info = "User update (method 3) failed: The updated age is not reflected in the database."
  )

  # Delete
  alice$delete()
  res = DBI::dbGetQuery(engine$get_connection(), 'select * from users where id = 1')
  expect_equal(
    nrow(res), 
    0,
    info = "User deletion failed: The user still exists in the database after deletion."
  )

  # Test error handling
  expect_error(
    User$create(id = 2, name = NULL, age = 25),
    "Column 'name' cannot be NULL",
    info = "Creating a user with a NULL name should raise an error."
  )

  # Test multiple user creation and retrieval
  User$create(id = 2, name = "Bob", age = 25)
  User$create(id = 3, name = "Charlie", age = 35)

  all_users = User$all()
  expect_equal(
    length(all_users),
    2,
    info = "Retrieving all users failed: Expected 2 users, but got a different number."
  )

  # Use expect_snapshot for complex output
  testthat::expect_snapshot(
    lapply(all_users, function(user) user$to_list()),
    info = "The list of all users does not match the expected snapshot."
  )
})