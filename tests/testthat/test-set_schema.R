test_that("set_schema updates table names for models and records", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    .schema = "public"
  )
  engine$dialect <- "postgres"

  model <- engine$model("users")
  expect_equal(model$tablename, "public.users")

  engine$schema <- "archive"
  expect_equal(model$tablename, "public.users")
  model$set_schema(engine$schema)
  expect_equal(model$tablename, "archive.users")

  record <- model$record()
  engine$schema <- "audit"
  record$set_schema(engine$schema)
  expect_equal(record$model$tablename, "audit.users")
})
