test_that("set_schema for sqlite creates schema-qualified tables", {
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:")
  model <- engine$model(
    "users",
    schema = "main",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT")
  )
  model$create_table(overwrite = TRUE)
  con <- engine$get_connection()
  expect_true(DBI::dbExistsTable(con, model$tablename))
  quoted <- DBI::dbQuoteIdentifier(con, model$tablename)
  expect_equal(model$tablename, "main.users")
  expect_equal(as.character(quoted), "`main.users`")
  engine$close()
})
