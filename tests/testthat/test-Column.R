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
  # when primary key is true, nullable gets set to null
  expect_null(col$nullable)
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

test_that("ForeignKey parses a plain 'table.column' reference", {
  fk <- ForeignKey("INTEGER", references = "users.id")
  expect_s3_class(fk, "ForeignKey")
  expect_null(fk$ref_schema)
  expect_equal(fk$ref_table, "users")
  expect_equal(fk$ref_column, "id")
  expect_equal(fk$references, "users.id")
})

test_that("ForeignKey parses a schema-qualified 'schema.table.column' reference", {
  fk <- ForeignKey("INTEGER", references = "other.users.id", on_delete = "CASCADE")
  expect_equal(fk$ref_schema, "other")
  expect_equal(fk$ref_table, "users")
  expect_equal(fk$ref_column, "id")
  expect_equal(fk$references, "other.users.id")
  expect_equal(fk$on_delete, "CASCADE")
})

test_that("ForeignKey accepts ref_schema passed explicitly", {
  fk <- ForeignKey("INTEGER", ref_schema = "other", ref_table = "users", ref_column = "id")
  expect_equal(fk$references, "other.users.id")
})

test_that("ForeignKey rejects malformed references", {
  expect_error(ForeignKey("INTEGER", references = "id"), "Expected")
  expect_error(ForeignKey("INTEGER", references = "a.b.c.d"), "Expected")
})

test_that("validate_identifier accepts schema-qualified names only when qualified = TRUE", {
  expect_equal(validate_identifier("users", qualified = FALSE), "users")
  expect_equal(validate_identifier("other.users", qualified = TRUE), "other.users")
  expect_error(validate_identifier("other.users", qualified = FALSE), "Invalid")
  expect_error(validate_identifier("a.b.c", qualified = TRUE), "Invalid")
})

test_that("render_constraint qualifies a cross-schema foreign key target", {
  fk <- ForeignKey("INTEGER", references = "other.users.id")
  fk$name <- "author_id"
  sql <- render_constraint(fk, DBI::ANSI())
  expect_match(sql, 'REFERENCES "other"."users"')
  expect_match(sql, '"id"')
})

test_that("render_constraint leaves a same-schema foreign key unqualified", {
  fk <- ForeignKey("INTEGER", references = "users.id")
  fk$name <- "user_id"
  sql <- render_constraint(fk, DBI::ANSI())
  expect_match(sql, 'REFERENCES "users"')
  expect_no_match(sql, '"\\."')
})

# =============================================================================
# REVIEW BUG: primary_key = FALSE wipes an explicit nullable (R/Column.R:32)
# =============================================================================
# Column() does `if (is.logical(primary_key)) nullable <- NULL`, which fires for
# FALSE as well as TRUE. The "a PK is implicitly NOT NULL" intent should only
# apply to TRUE (the guard should be isTRUE()). These tests assert the intended
# contract and currently fail, documenting the bug.

test_that("Column keeps nullable = FALSE when primary_key is explicitly FALSE", {
  col <- Column("INTEGER", primary_key = FALSE, nullable = FALSE)

  expect_s3_class(col, "Column")
  expect_false(col$primary_key)
  # nullable must survive: this column is non-PK but NOT NULL.
  expect_false(col$nullable)
})

test_that("render_field emits NOT NULL for a non-PK column declared nullable = FALSE", {
  col <- Column("INTEGER", primary_key = FALSE, nullable = FALSE)
  col$name <- "qty"
  sql <- render_field(col, DBI::ANSI())
  expect_match(sql, "NOT NULL")
})

