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

test_that("Record$update() modifies an existing row", {
    engine <- Engine$new(
        drv = RSQLite::SQLite(),
        dbname = ":memory:"
    )

    User <- engine$model(
        "users",
        id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE)
    )

    User$create_table()

    rec <- Record$new(User, id = 1, name = "Alice")
    rec$create()

    rec$update(name = "Alicia")

    result <- DBI::dbGetQuery(
        engine$get_connection(),
        "SELECT * FROM users WHERE id = 1"
    )

    expect_equal(result$name, "Alicia")

    engine$close()
})

test_that("Record$delete() removes a row from the database", {
    engine <- Engine$new(
        drv = RSQLite::SQLite(),
        dbname = ":memory:"
    )

    User <- engine$model(
        "users",
        id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE)
    )

    User$create_table()

    rec <- Record$new(User, id = 1, name = "Alice")
    rec$create()

    rec$delete()

    result <- DBI::dbGetQuery(
        engine$get_connection(),
        "SELECT * FROM users WHERE id = 1"
    )

    expect_equal(nrow(result), 0)

    engine$close()
})

test_that("Record$create() can take and implement a default function", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:"
  )

  User <- engine$model(
    "users",
    date = Column('TEXT', default = format(Sys.Date(), '%Y-%m-%d')),
    id = Column('INTEGER', primary_key = TRUE)
  )
  User$create_table()


  u1 = User$record(id=1)$create()
  expect_equal(u1$data$date, format(Sys.Date(), '%Y-%m-%d'))
  u1_read = User$read(id == 1, mode='get')
  expect_equal(u1_read$data$date, format(Sys.Date(), '%Y-%m-%d'))


})

test_that("Record$update() modifies existing rows and requires primary key", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:"
  )

  User <- engine$model(
    "users",
    id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
    name = Column("TEXT", nullable = FALSE)
  )
  User$create_table()

  rec <- User$record(id = 1, name = "Alice")
  rec$create()
  rec$update(name = "Alicia")

  result <- DBI::dbGetQuery(engine$get_connection(), "SELECT name FROM users WHERE id = 1")
  expect_equal(result$name, "Alicia")

  rec_no_pk <- User$record(name = "Bob")
  expect_error(rec_no_pk$update(name = "Bobby"), "primary key")

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

User |>
define_relationship(
  local_key='organization_id',
  type='many_to_one',
  related_model = Organization,
  related_key='id',
  ref='organization',
  backref='users')

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

test_that("Relationships work correctly and return appropriate types", {
  # Setup
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )

  User <- engine$model("users",
  id = Column("INTEGER", primary_key = TRUE),
  organization_id = ForeignKey("INTEGER", references = "organizations.id"),
  name = Column("VARCHAR", nullable = FALSE)
)

Organization <- engine$model("organizations",
id = Column("INTEGER", primary_key = TRUE),
name = Column("VARCHAR", nullable = FALSE)
)

Organization$create_table(overwrite = TRUE)
User$create_table(overwrite = TRUE)

# Insert data
org1 <- Organization$record(id = 100, name = "Data Corp")
org2 <- Organization$record(id = 101, name = "Tech Inc")
expect_no_error(org1$create())
expect_no_error(org2$create())

# Define relationships
User |>
define_relationship(
  local_key='organization_id',
  type='many_to_one',
  related_model = Organization,
  related_key='id',
  ref='organization',
  backref='users')

  # Insert users
  user1 <- User$record(id = 1, organization_id = 100, name = "Alice")
  user2 <- User$record(id = 2, organization_id = 100, name = "Bob")
  user3 <- User$record(id = 3, organization_id = 101, name = "Charlie")
  expect_no_error(user1$create())
  expect_no_error(user2$create())
  expect_no_error(user3$create())

  # Test many_to_one relationship (User to Organization)
  u1 <- User$read(id == 1, mode = "get")
  related_org <- u1$relationship('organization')
  expect_s3_class(related_org, "Record")
  expect_equal(related_org$data$id, 100)
  expect_equal(related_org$data$name, "Data Corp")

  # Test one_to_many relationship (Organization to Users)
  o1 <- Organization$read(id == 100, mode = "get")
  related_users <- o1$relationship('users')
  expect_type(related_users, "list")
  expect_length(related_users, 2)
  expect_s3_class(related_users[[1]], "Record")
  expect_equal(related_users[[1]]$data$name, "Alice")
  expect_equal(related_users[[2]]$data$name, "Bob")

  # Test one_to_one relationship
  # First, let's define a one_to_one relationship
  UserProfile <- engine$model("user_profiles",
  user_id = ForeignKey("INTEGER", references = "users.id", primary_key = TRUE),
  bio = Column("TEXT")
)
UserProfile$create_table(overwrite = TRUE)

User |>
define_relationship(
  local_key='id',
  type='one_to_one',
  related_model = UserProfile,
  related_key='user_id',
  ref='profile',
  backref='user')

  profile1 <- UserProfile$record(user_id = 1, bio = "Alice's bio")
  expect_no_error(profile1$create())

  u1_with_profile <- User$read(id == 1, mode = "get")
  related_profile <- u1_with_profile$relationship('profile')
  expect_s3_class(related_profile, "Record")
  expect_equal(related_profile$data$bio, "Alice's bio")

  # Test many_to_many relationship
  User |>
  define_relationship(
    local_key='id',
    type='many_to_many',
    related_model = User,
    related_key='id',
    ref='friends',
    backref='friends')

    u1$refresh()
    u1_friends <- u1$relationship('friends')
    expect_type(u1_friends, "list")

    # Clean up
    engine$close()
  })
