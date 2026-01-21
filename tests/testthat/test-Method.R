library(testthat)
library(oRm)

test_that("Method creates an orm_method object with function and target", {
  method <- Method(function() { "test" }, target = "record")

  expect_s3_class(method, "orm_method")
  expect_true(is.function(method$fn))
  expect_equal(method$target, "record")
  expect_null(method$name)  # Name is set by TableModel
})

test_that("Method validates target parameter", {
  expect_error(
    Method(function() { "test" }, target = "invalid"),
    "'arg' should be one of"
  )
})

test_that("Method validates fn parameter is a function", {
  expect_error(
    Method("not a function", target = "record"),
    "The 'fn' parameter must be a function."
  )
})

test_that("Method works with table target", {
  method <- Method(function() { "table method" }, target = "table")

  expect_s3_class(method, "orm_method")
  expect_equal(method$target, "table")
})

test_that("Method defaults to record target when not specified", {
  method <- Method(function() { "test" })

  expect_s3_class(method, "orm_method")
  expect_equal(method$target, "record")
})

test_that("TableModel extracts and stores Method objects", {
  # Create an in-memory SQLite engine for testing
  engine <- Engine$new("sqlite::memory:")

  # Define a model with methods
  Students <- engine$model(
    "students",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT"),
    greet = Method(function() {
      paste("Hello, I'm", self$data$name)
    }, target = "record"),
    find_by_name = Method(function(name) {
      self$read(name = name)
    }, target = "table")
  )

  # Check that methods are stored
  expect_equal(length(Students$methods), 2)
  expect_s3_class(Students$methods[[1]], "orm_method")
  expect_s3_class(Students$methods[[2]], "orm_method")

  # Check that method names are set
  expect_equal(Students$methods$greet$name, "greet")
  expect_equal(Students$methods$find_by_name$name, "find_by_name")
})

test_that("Table-targeted methods are injected into TableModel", {
  engine <- Engine$new("sqlite::memory:")

  Students <- engine$model(
    "students",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT"),
    find_all_students = Method(function() {
      self$read(.limit = NULL)
    }, target = "table")
  )

  # Check that the table method is available on the model
  expect_true(is.function(Students$find_all_students))
})

test_that("Record-targeted methods are injected into Record instances", {
  engine <- Engine$new("sqlite::memory:", drv=RSQLite::SQLite(), persist=TRUE)

  Students <- engine$model(
    "students",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT"),
    greet = Method(function() {
      paste("Hello, I'm", self$data$name)
    }, target = "record")
  )

  Students$create_table()

  # Create a record
  student <- Students$record(id = 1, name = "Alice")

  # Check that the record method is available
  expect_true(is.function(student$greet))

  # Test that the method works
  expect_equal(student$greet(), "Hello, I'm Alice")
})

test_that("Methods have access to self in record context", {
  engine <- Engine$new("sqlite::memory:", drv=RSQLite::SQLite(), persist=TRUE)

  Students <- engine$model(
    "students",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT"),
    present = Column("INTEGER", default = 0),
    mark_present = Method(function() {
      self$data$present <- 1
      return(self$data$present)
    }, target = "record")
  )

  Students$create_table()

  student <- Students$record(id = 1, name = "Bob")

  # Test that method can access and modify self
  result <- student$mark_present()
  expect_equal(result, 1)
  expect_equal(student$data$present, 1)
})

test_that("Methods have access to self in table context", {
  engine <- Engine$new("sqlite::memory:")

  Students <- engine$model(
    "students",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT"),
    get_table_name = Method(function() {
      self$tablename
    }, target = "table")
  )

  # Test that table method can access self
  table_name <- Students$get_table_name()
  expect_equal(table_name, "students")
})

test_that("Multiple methods can be defined on the same model", {
  engine <- Engine$new("sqlite::memory:", drv=RSQLite::SQLite(), persist=TRUE)

  Students <- engine$model(
    "students",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT"),
    age = Column("INTEGER"),
    greet = Method(function() {
      paste("Hello, I'm", self$data$name)
    }, target = "record"),
    get_age = Method(function() {
      self$data$age
    }, target = "record"),
    find_by_name = Method(function(name) {
      self$read(name = name)
    }, target = "table"),
    count_all = Method(function() {
      nrow(self$read(.mode = "data.frame", .limit = NULL))
    }, target = "table")
  )

  Students$create_table()

  # Check all methods are stored
  expect_equal(length(Students$methods), 4)

  # Check record methods
  student <- Students$record(id = 1, name = "Charlie", age = 20)
  expect_true(is.function(student$greet))
  expect_true(is.function(student$get_age))

  # Check table methods
  expect_true(is.function(Students$find_by_name))
  expect_true(is.function(Students$count_all))
})

test_that("Methods work correctly in integration scenario", {
  engine <- Engine$new("sqlite::memory:", drv=RSQLite::SQLite(), persist=TRUE)

  Students <- engine$model(
    "students",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT"),
    present = Column("INTEGER", default = 0),
    greet = Method(function() {
      paste("Hello, I'm", self$data$name)
    }, target = "record"),
    mark_present = Method(function() {
      self$data$present <- 1
      invisible(self)
    }, target = "record"),
    find_by_name = Method(function(name_val) {
      self$read(name == name_val)
    }, target = "table")
  )

  Students$create_table()

  # Create and save a student
  student <- Students$record(id = 1, name = "Diana")
  student$create()

  # Retrieve the student
  retrieved <- Students$get(id == 1)

  # Test record methods
  expect_equal(retrieved$greet(), "Hello, I'm Diana")

  # Test mark_present method
  retrieved$mark_present()
  expect_equal(retrieved$data$present, 1)

  # Update in database
  retrieved$update()

  # Test table method
  found_students <- Students$find_by_name("Diana")
  expect_equal(length(found_students), 1)
  expect_equal(found_students[[1]]$data$name, "Diana")
})
