library(testthat)

test_that("PostgreSQL engine initialized with .schema sets search_path", {
  executed <- NULL
  connect_args <- NULL
  engine <- NULL
  with_mocked_bindings({
    engine <<- Engine$new(drv = structure(list(), class = "PqDriver"), .schema = "public")
    engine$get_connection()
  },
  DBI::dbConnect = function(...) { connect_args <<- list(...); structure(list(), class = "PqConnection") },
  DBI::dbExecute = function(conn, sql) { executed <<- sql; 0 },
  DBI::dbQuoteIdentifier = function(conn, x) paste0('"', x, '"'))
  expect_equal(executed, 'SET search_path TO "public"')
  expect_false("schema" %in% names(connect_args))
})

test_that("model() without schema uses engine default", {
  engine <- Engine$new(drv = structure(list(), class = "PqDriver"), .schema = "public")
  model <- engine$model("users")
  expect_equal(model$schema, "public")
  expect_equal(model$tablename, "public.users")
})
