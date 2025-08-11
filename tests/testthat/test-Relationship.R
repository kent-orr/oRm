library(testthat)
library(oRm)

test_that("Should create a one-to-many relationship between two models correctly", {
  # Set up test models
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  User <- engine$model("users", id = Column("INTEGER", primary_key = TRUE))
  Post <- engine$model("posts", 
                       id = Column("INTEGER", primary_key = TRUE), 
                       user_id = Column("INTEGER"))

  # Define the relationship
  define_relationship(User, "id", "one_to_many", Post, "user_id", ref = "posts", backref = "user")

  # Check if the relationship is correctly set up in User model
  expect_true("posts" %in% names(User$relationships))
  expect_s3_class(User$relationships$posts, "Relationship")
  expect_equal(User$relationships$posts$type, "one_to_many")
  expect_equal(User$relationships$posts$local_key, "id")
  expect_equal(User$relationships$posts$related_key, "user_id")

  # Check if the reverse relationship is correctly set up in Post model
  expect_true("user" %in% names(Post$relationships))
  expect_s3_class(Post$relationships$user, "Relationship")
  expect_equal(Post$relationships$user$type, "many_to_one")
  expect_equal(Post$relationships$user$local_key, "user_id")
  expect_equal(Post$relationships$user$related_key, "id")

  # Clean up
  engine$close()
})

test_that("Should error when local_key or related_key do not exist", {
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  User <- engine$model("users", id = Column("INTEGER", primary_key = TRUE))
  Post <- engine$model("posts",
                       id = Column("INTEGER", primary_key = TRUE),
                       user_id = Column("INTEGER"))

  expect_error(
    define_relationship(User, "missing", "one_to_many", Post, "user_id"),
    "Field 'missing' not found in model 'users'"
  )

  expect_error(
    define_relationship(User, "id", "one_to_many", Post, "bad"),
    "Field 'bad' not found in model 'posts'"
  )

  engine$close()
})

test_that("Should error when foreign key references mismatch", {
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  User <- engine$model("users", id = Column("INTEGER", primary_key = TRUE))
  Category <- engine$model("categories", id = Column("INTEGER", primary_key = TRUE))
  Post <- engine$model("posts",
                       id = Column("INTEGER", primary_key = TRUE),
                       user_id = ForeignKey("INTEGER", references = "users.id"))

  expect_error(
    define_relationship(Post, "user_id", "many_to_one", Category, "id"),
    sprintf("Field 'user_id' must reference '%s.id'", Category$tablename)
  )

  engine$close()
})

test_that("Should handle creation of a many-to-many relationship with proper backref", {
  # Set up test models
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  Student <- engine$model("students", id = Column("INTEGER", primary_key = TRUE))
  Course <- engine$model("courses", id = Column("INTEGER", primary_key = TRUE))

  # Define the many-to-many relationship
  define_relationship(Student, "id", "many_to_many", Course, "id", ref = "courses", backref = "students")

  # Check if the relationship is correctly set up in Student model
  expect_true("courses" %in% names(Student$relationships))
  expect_s3_class(Student$relationships$courses, "Relationship")
  expect_equal(Student$relationships$courses$type, "many_to_many")
  expect_equal(Student$relationships$courses$local_key, "id")
  expect_equal(Student$relationships$courses$related_key, "id")

  # Check if the reverse relationship is correctly set up in Course model
  expect_true("students" %in% names(Course$relationships))
  expect_s3_class(Course$relationships$students, "Relationship")
  expect_equal(Course$relationships$students$type, "many_to_many")
  expect_equal(Course$relationships$students$local_key, "id")
  expect_equal(Course$relationships$students$related_key, "id")

  # Clean up
  engine$close()
})

test_that("Should throw an error when an invalid relationship type is provided", {
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  User <- engine$model("users", id = Column("INTEGER", primary_key = TRUE))
  Post <- engine$model("posts", id = Column("INTEGER", primary_key = TRUE), user_id = Column("INTEGER"))

  expect_error(
    define_relationship(User, "id", "invalid_type", Post, "user_id"),
    "'arg' should be one of \"one_to_one\", \"one_to_many\", \"many_to_many\", \"many_to_one\""
  )

  engine$close()
})

test_that("Should create default ref and backref names when not explicitly provided", {
  # Set up test models
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  Author <- engine$model("authors", id = Column("INTEGER", primary_key = TRUE))
  Book <- engine$model("books", 
                       id = Column("INTEGER", primary_key = TRUE), 
                       author_id = Column("INTEGER"))

  # Define the relationship without explicit ref and backref
  define_relationship(Author, "id", "one_to_many", Book, "author_id")

  # Check if default ref name is created in Author model
  expect_true("books" %in% names(Author$relationships))
  expect_s3_class(Author$relationships$books, "Relationship")
  expect_equal(Author$relationships$books$type, "one_to_many")

  # Check if default backref name is created in Book model
  expect_true("authors" %in% names(Book$relationships))
  expect_s3_class(Book$relationships$authors, "Relationship")
  expect_equal(Book$relationships$authors$type, "many_to_one")

  # Clean up
  engine$close()
})

test_that("Should not create a backref relationship when backref is set to FALSE", {
  # Set up test models
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  User <- engine$model("users", id = Column("INTEGER", primary_key = TRUE))
  Post <- engine$model("posts", 
                       id = Column("INTEGER", primary_key = TRUE), 
                       user_id = Column("INTEGER"))

  # Define the relationship with backref set to FALSE
  define_relationship(User, "id", "one_to_many", Post, "user_id", ref = "posts", backref = FALSE)

  # Check if the relationship is correctly set up in User model
  expect_true("posts" %in% names(User$relationships))
  expect_s3_class(User$relationships$posts, "Relationship")
  expect_equal(User$relationships$posts$type, "one_to_many")

  # Check that no reverse relationship is created in Post model
  expect_false("user" %in% names(Post$relationships))
  expect_equal(length(Post$relationships), 0)

  # Clean up
  engine$close()
})

test_that("Should correctly set up a one-to-one relationship with custom ref and backref names", {
  # Set up test models
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  User <- engine$model("users", id = Column("INTEGER", primary_key = TRUE))
  Profile <- engine$model("profiles", 
                          id = Column("INTEGER", primary_key = TRUE),
                          user_id = Column("INTEGER"))

  # Define the relationship with custom ref and backref names
  define_relationship(User, "id", "one_to_one", Profile, "user_id", ref = "user_profile", backref = "profile_owner")

  # Check if the relationship is correctly set up in User model
  expect_true("user_profile" %in% names(User$relationships))
  expect_s3_class(User$relationships$user_profile, "Relationship")
  expect_equal(User$relationships$user_profile$type, "one_to_one")
  expect_equal(User$relationships$user_profile$local_key, "id")
  expect_equal(User$relationships$user_profile$related_key, "user_id")

  # Check if the reverse relationship is correctly set up in Profile model
  expect_true("profile_owner" %in% names(Profile$relationships))
  expect_s3_class(Profile$relationships$profile_owner, "Relationship")
  expect_equal(Profile$relationships$profile_owner$type, "one_to_one")
  expect_equal(Profile$relationships$profile_owner$local_key, "user_id")
  expect_equal(Profile$relationships$profile_owner$related_key, "id")

  # Clean up
  engine$close()
})

test_that("Should handle multiple relationships defined on the same model", {
  # Set up test models
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  User <- engine$model("users", id = Column("INTEGER", primary_key = TRUE))
  Post <- engine$model("posts", 
                       id = Column("INTEGER", primary_key = TRUE), 
                       user_id = Column("INTEGER"))
  Comment <- engine$model("comments",
                          id = Column("INTEGER", primary_key = TRUE),
                          user_id = Column("INTEGER"),
                          post_id = Column("INTEGER"))

  # Define multiple relationships for the User model
  define_relationship(User, "id", "one_to_many", Post, "user_id", ref = "posts", backref = "author")
  define_relationship(User, "id", "one_to_many", Comment, "user_id", ref = "comments", backref = "commenter")

  # Check if both relationships are correctly set up in User model
  expect_true("posts" %in% names(User$relationships))
  expect_true("comments" %in% names(User$relationships))
  expect_s3_class(User$relationships$posts, "Relationship")
  expect_s3_class(User$relationships$comments, "Relationship")

  # Check if reverse relationships are correctly set up in Post and Comment models
  expect_true("author" %in% names(Post$relationships))
  expect_true("commenter" %in% names(Comment$relationships))

  # Verify the types of relationships
  expect_equal(User$relationships$posts$type, "one_to_many")
  expect_equal(User$relationships$comments$type, "one_to_many")
  expect_equal(Post$relationships$author$type, "many_to_one")
  expect_equal(Comment$relationships$commenter$type, "many_to_one")

  # Clean up
  engine$close()
})

test_that("Should correctly set up a many-to-one relationship with default naming", {
  # Set up test models
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  Department <- engine$model("departments", id = Column("INTEGER", primary_key = TRUE))
  Employee <- engine$model("employees", 
                           id = Column("INTEGER", primary_key = TRUE), 
                           department_id = Column("INTEGER"))

  # Define the relationship without specifying ref and backref
  define_relationship(Employee, "department_id", "many_to_one", Department, "id")

  # Check if the relationship is correctly set up in Employee model
  expect_true("departments" %in% names(Employee$relationships))
  expect_s3_class(Employee$relationships$departments, "Relationship")
  expect_equal(Employee$relationships$departments$type, "many_to_one")
  expect_equal(Employee$relationships$departments$local_key, "department_id")
  expect_equal(Employee$relationships$departments$related_key, "id")

  # Check if the reverse relationship is correctly set up in Department model
  expect_true("employees" %in% names(Department$relationships))
  expect_s3_class(Department$relationships$employees, "Relationship")
  expect_equal(Department$relationships$employees$type, "one_to_many")
  expect_equal(Department$relationships$employees$local_key, "id")
  expect_equal(Department$relationships$employees$related_key, "department_id")

  # Clean up
  engine$close()
})

test_that("Should maintain existing relationships when adding new ones to a model", {
  # Set up test models
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  User <- engine$model("users", id = Column("INTEGER", primary_key = TRUE))
  Post <- engine$model("posts", 
                       id = Column("INTEGER", primary_key = TRUE), 
                       user_id = Column("INTEGER"))
  Comment <- engine$model("comments",
                          id = Column("INTEGER", primary_key = TRUE),
                          user_id = Column("INTEGER"))

  # Define initial relationship
  define_relationship(User, "id", "one_to_many", Post, "user_id", ref = "posts", backref = "author")

  # Check initial relationship
  expect_true("posts" %in% names(User$relationships))
  expect_s3_class(User$relationships$posts, "Relationship")

  # Add new relationship
  define_relationship(User, "id", "one_to_many", Comment, "user_id", ref = "comments", backref = "commenter")

  # Check if both relationships exist
  expect_true("posts" %in% names(User$relationships))
  expect_true("comments" %in% names(User$relationships))
  expect_s3_class(User$relationships$posts, "Relationship")
  expect_s3_class(User$relationships$comments, "Relationship")

  # Verify the types and keys of both relationships
  expect_equal(User$relationships$posts$type, "one_to_many")
  expect_equal(User$relationships$posts$local_key, "id")
  expect_equal(User$relationships$posts$related_key, "user_id")
  expect_equal(User$relationships$comments$type, "one_to_many")
  expect_equal(User$relationships$comments$local_key, "id")
  expect_equal(User$relationships$comments$related_key, "user_id")

  # Clean up
  engine$close()
})

test_that("Relationship print method displays correct details", {
  # Set up test models
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  User <- engine$model("users", id = Column("INTEGER", primary_key = TRUE))
  Post <- engine$model("posts", 
                       id = Column("INTEGER", primary_key = TRUE), 
                       user_id = Column("INTEGER"))

  # Create a relationship
  relationship <- Relationship$new(User, "id", "one_to_many", Post, "user_id")

  # Capture the printed output
  output <- capture.output(relationship$print())

  # Check if the printed output matches the expected format
  expected_output <- "Relationship: one 'users.id' => many 'posts.user_id'"
  expect_equal(output, expected_output)

  # Clean up
  engine$close()
})