library(testthat)
library(oRm)

test_that("Column creates a basic object with only 'type' specified", {
  col <- Column("INTEGER")

  expect_s3_class(col, "Column")
  expect_equal(col$type, "INTEGER")
  expect_null(col$default)
  expect_null(col$primary_key)
  expect_null(col$nullable)
  expect_null(col$unique)
  expect_equal(col$extras, list())
})

test_that("Column correctly sets the 'primary_key' attribute to TRUE when specified", {
  col <- Column("INTEGER", primary_key = TRUE)

  expect_s3_class(col, "Column")
  expect_equal(col$type, "INTEGER")
  expect_true(col$primary_key)
  expect_null(col$nullable)  # Now NULL by default
})

test_that("Column sets 'nullable' to FALSE when explicitly specified", {
  col <- Column("TEXT", nullable = FALSE)

  expect_s3_class(col, "Column")
  expect_equal(col$type, "TEXT")
  expect_false(col$nullable)
})

test_that("Column creates an object with a default value when specified", {
  col <- Column("INTEGER", default = 42)

  expect_s3_class(col, "Column")
  expect_equal(col$type, "INTEGER")
  expect_equal(col$default, 42)
  expect_null(col$primary_key)
  expect_null(col$nullable)
  expect_null(col$unique)
  expect_equal(col$extras, list())
})

test_that("Column sets 'unique' attribute to TRUE when specified", {
  col <- Column("TEXT", unique = TRUE)

  expect_s3_class(col, "Column")
  expect_equal(col$type, "TEXT")
  expect_true(col$unique)
  expect_null(col$nullable)
  expect_null(col$primary_key)
  expect_null(col$default)
  expect_equal(col$extras, list())
})

test_that("Column correctly handles additional parameters passed through '...'", {
  col <- Column("TEXT", check = "length(value) > 5", collate = "NOCASE")

  expect_s3_class(col, "Column")
  expect_equal(col$type, "TEXT")
  expect_equal(col$extras, list(check = "length(value) > 5", collate = "NOCASE"))
  expect_true(is.list(col$extras))
  expect_equal(length(col$extras), 2)
  expect_equal(names(col$extras), c("check", "collate"))
  expect_equal(col$extras$check, "length(value) > 5")
  expect_equal(col$extras$collate, "NOCASE")
})

test_that("Column creates an object with all possible parameters specified", {
  col <- Column(
    type = "INTEGER",
    default = 0,
    primary_key = TRUE,
    nullable = FALSE,
    unique = TRUE,
    check = "value > 0",
    collate = "NOCASE"
  )

  expect_s3_class(col, "Column")
  expect_equal(col$type, "INTEGER")
  expect_equal(col$default, 0)
  expect_true(col$primary_key)
  expect_false(col$nullable)
  expect_true(col$unique)
  expect_equal(col$extras, list(check = "value > 0", collate = "NOCASE"))
})

test_that("Column maintains the correct class 'Column' for the returned object", {
  col <- Column("INTEGER")

  expect_s3_class(col, "Column")
  expect_true(inherits(col, "Column"))
  expect_equal(class(col), "Column")
})

test_that("Column handles NULL values for optional parameters", {
  col <- Column(
    "TEXT",
    default = NULL,
    primary_key = NULL,
    nullable = NULL,
    unique = NULL,
    check = NULL
  )

  expect_s3_class(col, "Column")
  expect_equal(col$type, "TEXT")
  expect_null(col$default)
  expect_null(col$primary_key)
  expect_null(col$nullable)
  expect_null(col$unique)
  expect_equal(col$extras, list(check = NULL))
})

