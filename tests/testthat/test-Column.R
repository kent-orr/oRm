library(testthat)
library(oRm)

test_that("Column creates a basic object with only 'type' specified", {
  col <- Column("INTEGER")
  
  expect_s3_class(col, "Column")
  expect_equal(col$type, "INTEGER")
  expect_null(col$default)
  expect_false(col$primary_key)
  expect_true(col$nullable)
  expect_false(col$unique)
  expect_null(col$foreign_key)
  expect_null(col$on_delete)
  expect_null(col$on_update)
  expect_equal(col$extras, list())
})

test_that("Column correctly sets the 'primary_key' attribute to TRUE when specified", {
  col <- Column("INTEGER", primary_key = TRUE)
  
  expect_s3_class(col, "Column")
  expect_equal(col$type, "INTEGER")
  expect_true(col$primary_key)
  expect_false(col$nullable)  # Primary keys are typically not nullable
})

test_that("Column sets 'nullable' to FALSE when explicitly specified", {
  col <- Column("TEXT", nullable = FALSE)
  
  expect_s3_class(col, "Column")
  expect_equal(col$type, "TEXT")
  expect_false(col$nullable)
})

test_that("Column correctly handles foreign key reference with 'on_delete' and 'on_update' options", {
  col <- Column("INTEGER", 
                foreign_key = "users.id", 
                on_delete = "CASCADE", 
                on_update = "RESTRICT")
  
  expect_s3_class(col, "Column")
  expect_equal(col$type, "INTEGER")
  expect_equal(col$foreign_key, "users.id")
  expect_equal(col$on_delete, "CASCADE")
  expect_equal(col$on_update, "RESTRICT")
  expect_true(col$nullable)
  expect_false(col$primary_key)
  expect_false(col$unique)
  expect_null(col$default)
})

test_that("Column creates an object with a default value when specified", {
  col <- Column("INTEGER", default = 42)
  
  expect_s3_class(col, "Column")
  expect_equal(col$type, "INTEGER")
  expect_equal(col$default, 42)
  expect_false(col$primary_key)
  expect_true(col$nullable)
  expect_false(col$unique)
  expect_null(col$foreign_key)
  expect_null(col$on_delete)
  expect_null(col$on_update)
  expect_equal(col$extras, list())
})

test_that("Column sets 'unique' attribute to TRUE when specified", {
  col <- Column("TEXT", unique = TRUE)
  
  expect_s3_class(col, "Column")
  expect_equal(col$type, "TEXT")
  expect_true(col$unique)
  expect_true(col$nullable)
  expect_false(col$primary_key)
  expect_null(col$default)
  expect_null(col$foreign_key)
  expect_null(col$on_delete)
  expect_null(col$on_update)
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
    foreign_key = "users.id",
    on_delete = "CASCADE",
    on_update = "RESTRICT",
    check = "value > 0",
    collate = "NOCASE"
  )
  
  expect_s3_class(col, "Column")
  expect_equal(col$type, "INTEGER")
  expect_equal(col$default, 0)
  expect_true(col$primary_key)
  expect_false(col$nullable)
  expect_true(col$unique)
  expect_equal(col$foreign_key, "users.id")
  expect_equal(col$on_delete, "CASCADE")
  expect_equal(col$on_update, "RESTRICT")
  expect_equal(col$extras, list(check = "value > 0", collate = "NOCASE"))
})

test_that("Column maintains the correct class 'Column' for the returned object", {
  col <- Column("INTEGER")
  
  expect_s3_class(col, "Column")
  expect_true(inherits(col, "Column"))
  expect_equal(class(col), "Column")
})

test_that("Column handles edge cases for optional parameters", {
  # Test with empty strings and NULL values for optional parameters
  col <- Column(
    "TEXT",
    default = "",
    primary_key = FALSE,
    nullable = TRUE,
    unique = FALSE,
    foreign_key = "",
    on_delete = NULL,
    on_update = "",
    check = NULL
  )
  
  expect_s3_class(col, "Column")
  expect_equal(col$type, "TEXT")
  expect_equal(col$default, "")
  expect_false(col$primary_key)
  expect_true(col$nullable)
  expect_false(col$unique)
  expect_equal(col$foreign_key, "")
  expect_null(col$on_delete)
  expect_equal(col$on_update, "")
  expect_equal(col$extras, list(check = NULL))
})