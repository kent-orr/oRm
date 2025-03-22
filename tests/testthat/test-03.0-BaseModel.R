test_that("BaseModel initializes and defines fields correctly", {
  skip_if(Sys.getenv("DB_USER") == "" || Sys.getenv("DB_PASS") == "",
          "DB_USER or DB_PASS not set")

  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:"
  )

  model <- BaseModel$new(
    tablename = "test_basemodel",
    engine = engine,
    id = Column("INTEGER", key = TRUE, nullable = FALSE),
    name = Column("TEXT", nullable = FALSE),
    created_at = Column("TIMESTAMP")
  )

  # Check table name and engine binding
  expect_equal(model$tablename, "test_basemodel")
  expect_identical(model$engine, engine)

  # Check that all expected fields are present
  expect_setequal(names(model$fields), c("id", "name", "created_at"))

  # Check field types
  dbi_fields <- model$get_fields_for_dbi()
  expect_type(dbi_fields, "character")
  expect_equal(dbi_fields[["id"]], "INTEGER")
  expect_equal(dbi_fields[["name"]], "TEXT")

  # Create the table in the DB
  con <- model$get_connection()
  expect_true(DBI::dbIsValid(con))

  DBI::dbExecute(con, "DROP TABLE IF EXISTS test_basemodel")
  model$create_table()
  expect_true("test_basemodel" %in% DBI::dbListTables(con))

  # Print shouldn't error
  # expect_no_error(invisible(model$print()))

  # Clean up
  DBI::dbExecute(con, "DROP TABLE IF EXISTS test_basemodel")
  engine$close()
})
