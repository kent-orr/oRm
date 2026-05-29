# Tests for Engine$hydrate() and the reflection helpers it relies on.
#
# These tests deliberately verify persistence with direct DBI queries rather
# than TableModel$read(), so they exercise hydrate's own responsibilities
# (column reflection + model construction) without depending on the dbplyr
# read path.

test_that("hydrate reflects column names from an existing table", {
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  on.exit(engine$close(), add = TRUE)

  engine$model(
    "users",
    id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
    name = Column("TEXT", nullable = FALSE),
    age = Column("INTEGER"),
    hash = Column("TEXT")
  )$create_table()

  Users <- engine$hydrate("users")
  expect_s3_class(Users, "TableModel")
  expect_equal(names(Users$fields), c("id", "name", "age", "hash"))
  expect_true(all(vapply(Users$fields, inherits, logical(1), "Column")))
})

test_that("a hydrated model supports create without predefining columns", {
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  on.exit(engine$close(), add = TRUE)
  con <- engine$get_connection()

  engine$model(
    "users",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT"),
    age = Column("INTEGER")
  )$create_table()

  Users <- engine$hydrate("users")
  Users$record(id = 1L, name = "Kent", age = 34L)$create()
  Users$record(id = 2L, name = "Dylan", age = 25L)$create()

  rows <- DBI::dbGetQuery(con, "SELECT id, name, age FROM users ORDER BY id")
  expect_equal(rows$id, c(1, 2))
  expect_equal(rows$name, c("Kent", "Dylan"))
})

test_that("update and delete on a hydrated model work when a primary key is supplied via ...", {
  # The generic-baseline hydrate does not reflect primary keys, so update()/
  # delete() (which key off declared PK fields) require the caller to supply
  # the key column via ... until per-dialect PK reflection lands.
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  on.exit(engine$close(), add = TRUE)
  con <- engine$get_connection()

  engine$model(
    "users",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT")
  )$create_table()

  Users <- engine$hydrate("users", id = Column("INTEGER", primary_key = TRUE))
  Users$record(id = 1L, name = "Kent")$create()

  rec <- Users$record(id = 1L, name = "Kent")
  rec$update(name = "Hogan")
  expect_equal(
    DBI::dbGetQuery(con, "SELECT name FROM users WHERE id = 1")$name,
    "Hogan"
  )

  rec$delete()
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM users")$n, 0)
})

test_that("a hydrated model reads back records via the normal read() API", {
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  on.exit(engine$close(), add = TRUE)

  engine$model(
    "users",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT"),
    age = Column("INTEGER")
  )$create_table()

  Users <- engine$hydrate("users")
  Users$record(id = 1L, name = "Kent", age = 34L)$create()
  Users$record(id = 2L, name = "Dylan", age = 25L)$create()

  df <- Users$read(.mode = "data.frame")
  expect_equal(nrow(df), 2)
  expect_setequal(names(df), c("id", "name", "age"))

  kent <- Users$read(id == 1, .mode = "get")
  expect_equal(kent$data$name, "Kent")
})

test_that("hydrate respects include and exclude", {
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  on.exit(engine$close(), add = TRUE)

  engine$model(
    "items",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT"),
    hash = Column("TEXT"),
    configuration = Column("TEXT")
  )$create_table()

  # include keeps only requested columns, in table order
  inc <- engine$hydrate("items", include = c("name", "id"))
  expect_equal(names(inc$fields), c("id", "name"))

  # exclude drops requested columns
  exc <- engine$hydrate("items", exclude = c("hash", "configuration"))
  expect_equal(names(exc$fields), c("id", "name"))

  # include + exclude: include first, then exclude
  both <- engine$hydrate("items", include = c("id", "name", "hash"), exclude = "hash")
  expect_equal(names(both$fields), c("id", "name"))
})

test_that("hydrate warns on missing include columns and errors when all filtered out", {
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  on.exit(engine$close(), add = TRUE)

  engine$model(
    "things",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT")
  )$create_table()

  expect_warning(
    m <- engine$hydrate("things", include = c("id", "nope")),
    "include columns not found"
  )
  expect_equal(names(m$fields), "id")

  expect_error(
    suppressWarnings(engine$hydrate("things", include = "nope")),
    "no columns remain"
  )
})

test_that("hydrate errors on a nonexistent table", {
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  on.exit(engine$close(), add = TRUE)

  expect_error(engine$hydrate("does_not_exist"), "could not read table")
})

test_that("hydrate lets ... override reflected columns and attach methods", {
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  on.exit(engine$close(), add = TRUE)

  engine$model(
    "people",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT")
  )$create_table()

  People <- engine$hydrate(
    "people",
    id = Column("INTEGER", primary_key = TRUE),
    greet = Method(function() paste("hi", self$data$name), target = "record")
  )

  # Overridden id column carries the primary_key attribute from ...
  expect_true(isTRUE(People$fields$id$primary_key))
  # Attached record method is available on records
  rec <- People$record(id = 1L, name = "Ada")
  expect_equal(rec$greet(), "hi Ada")
})

test_that("reflect_columns + filter_reflected_columns behave as units", {
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  on.exit(engine$close(), add = TRUE)

  engine$model(
    "widgets",
    id = Column("INTEGER", primary_key = TRUE),
    label = Column("TEXT")
  )$create_table()

  cols <- reflect_columns(engine, "widgets")
  expect_equal(names(cols), c("id", "label"))
  expect_true(all(vapply(cols, inherits, logical(1), "Column")))

  expect_equal(names(filter_reflected_columns(cols, include = "label")), "label")
  expect_equal(names(filter_reflected_columns(cols, exclude = "id")), "label")
})
