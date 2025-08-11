test_that("SQLite dialect treats schema.table as single identifier", {
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:")
  model <- engine$model("users", schema = "main")
  con <- engine$get_connection()
  quoted <- DBI::dbQuoteIdentifier(con, model$tablename)
  expect_equal(model$tablename, "main.users")
  expect_equal(as.character(quoted), "`main.users`")
  engine$close()
})
