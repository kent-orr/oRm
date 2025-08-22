library(testthat)

test_that("Engine with schema parameter stores schema correctly", {
  # Create a mock driver for testing schema functionality
  mock_driver <- structure(list(), class = "PqDriver")
  
  # Create engine with schema - this should store the schema internally
  engine <- Engine$new(drv = mock_driver, .schema = "public")
  
  # Test that schema is stored correctly
  expect_equal(engine$schema, "public")
})

test_that("model() without schema uses engine default", {
  # Create a mock driver for testing
  mock_driver <- structure(list(), class = "PqDriver")
  engine <- Engine$new(drv = mock_driver, .schema = "public")
  
  model <- engine$model("users")
  expect_equal(model$schema, "public")
  expect_equal(model$tablename, "public.users")
})
