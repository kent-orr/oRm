test_that("set_schema prefixes schema for postgres and mysql", {
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:")
  engine$dialect <- "postgres"
  model_pg <- engine$model("users", schema = "public")
  expect_equal(model_pg$tablename, "public.users")
  engine$dialect <- "mysql"
  model_my <- engine$model("users", schema = "test")
  expect_equal(model_my$tablename, "test.users")
})

test_that("set_schema for sqlite treats schema.table as single identifier", {
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:")
  model <- engine$model("users", schema = "main")
  con <- engine$get_connection()
  quoted <- DBI::dbQuoteIdentifier(con, model$tablename)
  expect_equal(model$tablename, "main.users")
  expect_equal(as.character(quoted), "`main.users`")
  engine$close()
})
