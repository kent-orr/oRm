testthat::source_test_helpers()

# Dialect selection. `odbc::odbc()` presents as class "OdbcDriver" no matter
# which DBMS it targets, so SQL Server cannot be identified from the driver
# class alone. Detection therefore sniffs the connection arguments, and
# `.dialect` provides an explicit override for cases sniffing cannot reach.
#
# These construct Engines but never connect, so no driver packages are needed.

fake_odbc_drv <- function() structure(list(), class = "OdbcDriver")

test_that("driver-class detection still works for the built-in dialects", {
  skip_if_not_installed("RSQLite")
  expect_equal(Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:")$dialect, "sqlite")
})

test_that(".dialect sets the dialect explicitly", {
  engine <- Engine$new(drv = fake_odbc_drv(), .dialect = "mssql")
  expect_equal(engine$dialect, "mssql")
})

test_that(".dialect overrides driver-class detection", {
  skip_if_not_installed("RSQLite")
  # Used by the mssql unit tests to generate T-SQL over a real SQLite
  # connection, so this override must win.
  engine <- Engine$new(
    drv = RSQLite::SQLite(), dbname = ":memory:", .dialect = "mssql"
  )
  expect_equal(engine$dialect, "mssql")
})

test_that(".dialect must be a single non-empty string", {
  expect_error(Engine$new(drv = fake_odbc_drv(), .dialect = 1), "\\.dialect")
  expect_error(Engine$new(drv = fake_odbc_drv(), .dialect = c("a", "b")), "\\.dialect")
  expect_error(Engine$new(drv = fake_odbc_drv(), .dialect = NA_character_), "\\.dialect")
})

test_that("odbc connections are detected as mssql from the Driver argument", {
  expect_equal(
    Engine$new(
      drv = fake_odbc_drv(),
      Driver = "ODBC Driver 18 for SQL Server",
      Server = "localhost"
    )$dialect,
    "mssql"
  )

  # Lowercase argument name and driver name.
  expect_equal(
    Engine$new(drv = fake_odbc_drv(), driver = "sql server")$dialect,
    "mssql"
  )

  # Via a full connection string.
  expect_equal(
    Engine$new(
      drv = fake_odbc_drv(),
      .connection_string = "Driver={ODBC Driver 18 for SQL Server};Server=localhost;"
    )$dialect,
    "mssql"
  )
})

test_that("odbc connections to other databases are not claimed as mssql", {
  expect_equal(
    Engine$new(drv = fake_odbc_drv(), Driver = "PostgreSQL Unicode")$dialect,
    "default"
  )
  # No hint at all: fall back to "default" rather than guessing.
  expect_equal(Engine$new(drv = fake_odbc_drv())$dialect, "default")
})
