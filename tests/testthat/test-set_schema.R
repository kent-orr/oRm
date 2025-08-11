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

test_that("engine$set_schema updates subsequent models", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:"
  )
  engine$dialect <- "postgres"

  with_mocked_bindings(
    `engine.schema::set_schema` = function(conn, dialect, schema) NULL,
    engine$set_schema("analytics")
  )

  model <- engine$model("users")
  expect_equal(model$tablename, "analytics.users")
})

test_that("set_schema issues dialect specific SQL", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:"
  )

  engine$dialect <- "postgres"
  pg_sql <- NULL
  with_mocked_bindings(
    `engine.schema::set_schema` = function(conn, dialect, schema) {
      pg_sql <<- paste("SET search_path TO", schema)
    },
    engine$set_schema("reporting")
  )
  expect_equal(pg_sql, "SET search_path TO reporting")

  engine$dialect <- "mysql"
  my_sql <- NULL
  with_mocked_bindings(
    `engine.schema::set_schema` = function(conn, dialect, schema) {
      my_sql <<- paste("USE", schema)
    },
    engine$set_schema("reporting")
  )
  expect_equal(my_sql, "USE reporting")
})

test_that("SQLite warns when schema is ignored", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:"
  )
  engine$dialect <- "sqlite"

  with_mocked_bindings(
    `engine.schema::set_schema` = function(conn, dialect, schema) {
      warning("SQLite does not support schemas")
    },
    expect_warning(engine$set_schema("ignored"), "does not support")
  )

  expect_warning(
    model <- engine$model("users"),
    "SQLite does not support schema"
  )
  expect_equal(model$tablename, "users")
})

