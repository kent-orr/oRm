# Tests for Engine(.read_only = TRUE).
# Verifies both layers:
#   1. Application-level guards on Engine$execute, Record$*, TableModel$*.
#   2. Connection-level enforcement applied per dialect.

# ---------------------------------------------------------------------------
# is_read_sql helper
# ---------------------------------------------------------------------------

test_that("is_read_sql classifies common statements correctly", {
  is_read_sql <- oRm:::is_read_sql

  expect_true(is_read_sql("SELECT 1"))
  expect_true(is_read_sql("  select * from t"))
  expect_true(is_read_sql("WITH x AS (SELECT 1) SELECT * FROM x"))
  expect_true(is_read_sql("EXPLAIN SELECT * FROM t"))
  expect_true(is_read_sql("PRAGMA table_info(t)"))
  expect_true(is_read_sql("-- a comment\nSELECT 1"))

  expect_false(is_read_sql("INSERT INTO t VALUES (1)"))
  expect_false(is_read_sql("UPDATE t SET x=1"))
  expect_false(is_read_sql("DELETE FROM t"))
  expect_false(is_read_sql("DROP TABLE t"))
  expect_false(is_read_sql("CREATE TABLE t (id INT)"))
  expect_false(is_read_sql("TRUNCATE t"))
})

# ---------------------------------------------------------------------------
# SQLite: full coverage (app-level guard + driver-level SQLITE_RO flag)
# ---------------------------------------------------------------------------

# Build a populated SQLite file we can reopen in read-only mode.
seed_sqlite_db <- function() {
  db_path <- tempfile(fileext = ".sqlite")
  rw <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = db_path,
    persist = TRUE
  )
  Widget <- rw$model(
    "widgets",
    id   = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT", nullable = FALSE)
  )
  Widget$create_table()
  Widget$record(id = 1, name = "alpha")$create()
  Widget$record(id = 2, name = "beta")$create()
  rw$close()
  db_path
}

ro_sqlite_engine <- function(db_path, persist = TRUE) {
  Engine$new(
    drv = RSQLite::SQLite(),
    dbname = db_path,
    persist = persist,
    .read_only = TRUE
  )
}

test_that("SQLite read-only engine stores the flag and injects SQLITE_RO", {
  skip_if_not_installed("RSQLite")

  db_path <- seed_sqlite_db()
  on.exit(unlink(db_path), add = TRUE)

  ro <- ro_sqlite_engine(db_path)
  on.exit(ro$close(), add = TRUE)

  expect_true(ro$read_only)
  expect_equal(ro$conn_args$flags, RSQLite::SQLITE_RO)
})

test_that("SQLite read-only engine permits reads", {
  skip_if_not_installed("RSQLite")

  db_path <- seed_sqlite_db()
  on.exit(unlink(db_path), add = TRUE)

  ro <- ro_sqlite_engine(db_path)
  on.exit(ro$close(), add = TRUE)

  Widget <- ro$model(
    "widgets",
    id   = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT")
  )

  rec <- Widget$get(id == 1)
  expect_equal(rec$data$name, "alpha")
  expect_length(Widget$all(), 2)
})

test_that("SQLite read-only engine refuses writes via app-level guards", {
  skip_if_not_installed("RSQLite")

  db_path <- seed_sqlite_db()
  on.exit(unlink(db_path), add = TRUE)

  ro <- ro_sqlite_engine(db_path)
  on.exit(ro$close(), add = TRUE)

  Widget <- ro$model(
    "widgets",
    id   = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT", nullable = FALSE)
  )

  expect_error(ro$execute("DELETE FROM widgets"), "read-only")
  expect_error(ro$execute("UPDATE widgets SET name='x' WHERE id=1"), "read-only")
  expect_error(
    ro$execute("INSERT INTO widgets (id, name) VALUES (3, 'gamma')"),
    "read-only"
  )

  expect_error(Widget$record(id = 3, name = "gamma")$create(), "read-only")
  expect_error(Widget$create_table(overwrite = TRUE, ask = FALSE), "read-only")
  expect_error(Widget$drop_table(ask = FALSE), "read-only")

  rec <- Widget$get(id == 1)
  expect_error(rec$update(name = "renamed"), "read-only")
  expect_error(rec$delete(), "read-only")
})

test_that("SQLite SQLITE_RO flag rejects writes even when bypassing app guards", {
  skip_if_not_installed("RSQLite")

  db_path <- seed_sqlite_db()
  on.exit(unlink(db_path), add = TRUE)

  ro <- ro_sqlite_engine(db_path)
  on.exit(ro$close(), add = TRUE)

  con <- ro$get_connection()
  # Direct DBI call sidesteps Engine$execute, so any failure here is from the
  # SQLite driver itself rejecting the write attempt.
  expect_error(
    DBI::dbExecute(con, "INSERT INTO widgets (id, name) VALUES (3, 'gamma')"),
    "readonly|read.only|attempt to write",
    ignore.case = TRUE
  )
})

test_that("default SQLite engine (no .read_only) still allows writes", {
  skip_if_not_installed("RSQLite")

  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )
  on.exit(engine$close(), add = TRUE)

  expect_false(engine$read_only)
  expect_null(engine$conn_args$flags)

  Widget <- engine$model("widgets", id = Column("INTEGER", primary_key = TRUE))
  expect_no_error(Widget$create_table())
  expect_no_error(Widget$record(id = 1)$create())
})

# ---------------------------------------------------------------------------
# PostgreSQL: connection-level enforcement via session characteristics
# ---------------------------------------------------------------------------

test_that("PostgreSQL read-only engine refuses writes at the driver level", {
  skip_on_cran()
  skip_if_not_installed("RPostgres")

  conn_info <- tryCatch({
    use_postgres_test_db()
  }, error = function(e) {
    testthat::skip(paste("Could not connect to PostgreSQL test database:", e$message))
  })
  withr::defer(clear_postgres_test_tables())

  # Seed a table on a writable engine.
  rw <- do.call(Engine$new, conn_info)
  withr::defer(rw$close())
  Widget <- rw$model(
    "ro_widgets",
    id   = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT")
  )
  Widget$create_table()
  Widget$record(id = 1, name = "alpha")$create()
  rw$close()

  # Reopen as read-only.
  ro <- do.call(Engine$new, c(conn_info, list(.read_only = TRUE, persist = TRUE)))
  withr::defer(ro$close())

  expect_true(ro$read_only)

  # Reads still work.
  WidgetRO <- ro$model(
    "ro_widgets",
    id   = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT")
  )
  expect_equal(WidgetRO$get(id == 1)$data$name, "alpha")

  # App-level guard refuses non-SELECT.
  expect_error(ro$execute("DELETE FROM ro_widgets"), "read-only")

  # Driver-level: bypass app guard with a direct DBI call. PostgreSQL should
  # raise its own error citing the read-only transaction.
  con <- ro$get_connection()
  expect_error(
    DBI::dbExecute(con, "INSERT INTO ro_widgets (id, name) VALUES (2, 'beta')"),
    "read-only"
  )
})

# ---------------------------------------------------------------------------
# MySQL: no test infra available, so verify the dispatch + emitted SQL.
# ---------------------------------------------------------------------------

test_that("apply_read_only.mysql emits SET SESSION TRANSACTION READ ONLY", {
  skip_if_not_installed("testthat", "3.5.0")

  captured <- list()
  testthat::local_mocked_bindings(
    dbExecute = function(conn, statement, ...) {
      captured[[length(captured) + 1]] <<- statement
      1L
    },
    .package = "DBI"
  )

  fake_engine <- structure(
    list(dialect = "mysql"),
    class = "Engine"
  )
  fake_con <- structure(list(), class = "FakeConn")

  oRm:::apply_read_only(fake_engine, fake_con)

  expect_length(captured, 1)
  expect_match(
    captured[[1]],
    "SET\\s+SESSION\\s+TRANSACTION\\s+READ\\s+ONLY",
    ignore.case = TRUE
  )
})

test_that("apply_read_only.default is a no-op", {
  fake_engine <- structure(list(dialect = "default"), class = "Engine")
  expect_null(oRm:::apply_read_only(fake_engine, NULL))
})
