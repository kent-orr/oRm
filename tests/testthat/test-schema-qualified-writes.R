library(testthat)

# Regression tests for schema-qualified table names on the non-flush write
# paths. model$tablename is stored as a plain "schema.table" string; passing
# that string straight to DBI (dbAppendTable, dbQuoteIdentifier) quotes it as
# a single identifier whose name literally contains a dot, so the relation is
# never found. SQLite's built-in "main" schema reproduces this without needing
# a Postgres server.

test_that("Record$create() inside a transaction inserts into a schema-qualified table", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )
  withr::defer(engine$close())

  User <- engine$model(
    "main.users",
    id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
    name = Column("TEXT", nullable = FALSE)
  )
  User$create_table()

  # Inside a transaction create() defaults to the non-flush dbAppendTable path
  with(engine, {
    Record$new(User, id = 1, name = "Alice")$create()
  })

  result <- DBI::dbGetQuery(
    engine$get_connection(),
    'SELECT * FROM "main"."users"'
  )
  expect_equal(nrow(result), 1)
  expect_equal(result$name, "Alice")
})

test_that("Record$create(flush_record = FALSE) inserts into a schema-qualified table", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )
  withr::defer(engine$close())

  User <- engine$model(
    "main.users",
    id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
    name = Column("TEXT", nullable = FALSE)
  )
  User$create_table()

  Record$new(User, id = 1, name = "Bob")$create(flush_record = FALSE)

  result <- DBI::dbGetQuery(
    engine$get_connection(),
    'SELECT * FROM "main"."users"'
  )
  expect_equal(nrow(result), 1)
  expect_equal(result$name, "Bob")
})

test_that("create_table(overwrite = TRUE) drops a schema-qualified table", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )
  withr::defer(engine$close())

  User <- engine$model(
    "main.users",
    id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
    name = Column("TEXT", nullable = FALSE)
  )
  User$create_table()
  Record$new(User, id = 1, name = "Alice")$create()

  # Recreating with overwrite must drop the qualified table, not silently
  # target a relation named "main.users"
  User$create_table(overwrite = TRUE, ask = FALSE)

  result <- DBI::dbGetQuery(
    engine$get_connection(),
    'SELECT * FROM "main"."users"'
  )
  expect_equal(nrow(result), 0)
})

test_that("postgres: Record$create() inside a transaction inserts into a schema-qualified table", {
  conn_info <- tryCatch({
    use_postgres_test_db()
  }, error = function(e) {
    testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
  })
  withr::defer(clear_postgres_test_tables())

  engine <- do.call(Engine$new, conn_info)
  withr::defer(engine$close())

  engine$create_schema("qualified_writes")
  withr::defer(
    DBI::dbExecute(
      engine$get_connection(),
      "DROP SCHEMA IF EXISTS qualified_writes CASCADE"
    )
  )

  Product <- engine$model(
    "products",
    id = Column("SERIAL", primary_key = TRUE),
    name = Column("TEXT", nullable = FALSE),
    .schema = "qualified_writes"
  )
  Product$create_table()

  with(engine, {
    Record$new(Product, name = "Widget")$create()
  })

  result <- DBI::dbGetQuery(
    engine$get_connection(),
    'SELECT * FROM "qualified_writes"."products"'
  )
  expect_equal(nrow(result), 1)
  expect_equal(result$name, "Widget")
})
