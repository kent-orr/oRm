test_that("SQLite dialect handles auto-increment and defaults", {
  skip_if_not_installed("RSQLite")

  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )
  expect_equal(engine$dialect, "sqlite")

  Example <- engine$model(
    "test_table",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT", default = "anon")
  )
  Example$create_table(overwrite = TRUE)

  rec1 <- Example$record(name = "alpha")
  rec1$create()
  expect_equal(rec1$data$id, 1L)
  expect_equal(rec1$data$name, "alpha")

  rec2 <- Example$record()
  rec2$data$name <- NULL
  rec2$create()
  expect_equal(rec2$data$id, 2L)
  expect_equal(rec2$data$name, "anon")

  all_records <- Example$read(.mode = "all")
  expect_equal(length(all_records), 2L)
  expect_equal(all_records[[1]]$data$id, 1L)
  expect_equal(all_records[[1]]$data$name, "alpha")
  expect_equal(all_records[[2]]$data$id, 2L)
  expect_equal(all_records[[2]]$data$name, "anon")

    flush_res <- oRm:::flush(engine, Example$tablename, list(name = "beta"), engine$get_connection())
    expect_equal(flush_res$name, "beta")
    expect_equal(flush_res$id, 3L)

    expect_warning(
        schema_res <- engine$check_schema_exists("ignored"),
        "SQLite does not support schemas"
    )
    expect_true(schema_res)

    Example$drop_table(ask = FALSE)
    engine$close()
})

# =============================================================================
# with.Engine TRANSACTION BUGS (SQLite-specific manifestations)
# =============================================================================
# These tests assert the *intended* contract of with.Engine. They currently
# fail, documenting bugs surfaced in review. SQLite's transaction semantics
# differ from Postgres, so the failure modes (and assertions) differ too.

test_that("SQLite: nested with.Engine should not error (bug: nesting)", {
  skip_if_not_installed("RSQLite")
  # On SQLite a nested dbBegin() raises a hard error
  # ("cannot start a transaction within a transaction"), so a with.Engine
  # nested inside another -- directly or via a helper -- is currently
  # impossible. Correct behaviour: the inner block runs as a savepoint and
  # everything commits together.
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  model <- engine$model(
    "test_table",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT")
  )
  model$create_table(overwrite = TRUE)

  expect_no_error(
    with.Engine(engine, {
      model$record(id = 1, name = "Alice")$create()
      with.Engine(engine, {
        model$record(id = 2, name = "Bob")$create()
      })
      # The outer transaction must survive the inner block returning.
      expect_true(engine$get_transaction_state())
      model$record(id = 3, name = "Carol")$create()
    })
  )

  expect_equal(length(model$read()), 3L)
  expect_false(engine$get_transaction_state())

  engine$close()
})

test_that("SQLite: with.Engine commits through a pooled engine (bug: pooling)", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("pool")
  # get_connection() hands back the Pool object, so dbBegin()/dbCommit() are not
  # applied to a single checked-out connection. A file-backed DB is used so the
  # table is shared across pool checkouts.
  dbfile <- tempfile(fileext = ".sqlite")
  on.exit(unlink(dbfile), add = TRUE)

  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = dbfile, use_pool = TRUE)
  model <- engine$model(
    "test_table",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT")
  )
  model$create_table(overwrite = TRUE)

  expect_no_error(
    with.Engine(engine, {
      model$record(id = 1, name = "Alice")$create()
      model$record(id = 2, name = "Bob")$create()
    })
  )

  expect_equal(length(model$read()), 2L)

  engine$close()
})

test_that("SQLite: with.Engine refuses a transaction on a read-only engine (bug: read_only)", {
  skip_if_not_installed("RSQLite")
  # with.Engine opens a transaction on a read-only engine and only fails later,
  # mid-block, when a write is attempted. It should refuse upfront.
  dbfile <- tempfile(fileext = ".sqlite")
  on.exit(unlink(dbfile), add = TRUE)

  seed <- Engine$new(drv = RSQLite::SQLite(), dbname = dbfile)
  seed$model(
    "test_table",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT")
  )$create_table(overwrite = TRUE)
  seed$close()

  ro <- Engine$new(drv = RSQLite::SQLite(), dbname = dbfile, .read_only = TRUE)

  expect_error(
    with.Engine(ro, { "noop" }),
    regexp = "read-only"
  )

  ro$close()
})
