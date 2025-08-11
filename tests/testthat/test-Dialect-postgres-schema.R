test_that("Postgres dialect prefixes schema in table name", {
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:")
  engine$dialect <- "postgres"
  model <- engine$model("users", schema = "public")
  expect_equal(model$tablename, "public.users")
})
