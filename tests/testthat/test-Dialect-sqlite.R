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

# ---------------------------------------------------------------------------
# .autoflush ON SQLITE
#
# SQLite has no RETURNING/OUTPUT clause in the path oRm takes; the key comes
# back via a last-rowid lookup, so the three flush levels are asserted here
# against the dialect rather than only through the generic Engine tests.
# ---------------------------------------------------------------------------

test_that("SQLite: .autoflush returns AUTOINCREMENT keys inside a transaction", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )
  withr::defer(engine$close())

  parent <- engine$model(
    "flush_orders",
    id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
    customer = Column("TEXT")
  )
  child <- engine$model(
    "flush_order_items",
    id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
    order_id = Column("INTEGER"),
    sku = Column("TEXT")
  )
  parent$create_table()
  child$create_table()

  # Outside a transaction a create always flushes.
  outside <- parent$record(customer = "outside")$create()
  expect_false(is.null(outside$data$id))

  # Inside one the default is a plain insert.
  plain <- with(engine, parent$record(customer = "plain")$create())
  expect_null(plain$data$id)

  # Block level: the parent key reaches the child insert in the same block.
  item <- with(engine, {
    order <- parent$record(customer = "Alice")$create()
    expect_false(is.null(order$data$id))
    child$record(order_id = order$data$id, sku = "A-1")$create()
  }, .autoflush = TRUE)

  expect_false(is.null(item$data$id))
  expect_equal(
    item$data$order_id,
    parent$read(customer == "Alice", .mode = "data.frame")$id
  )

  # Record level wins over a block that opted out.
  explicit <- with(engine, {
    bulk <- parent$record(customer = "bulk")$create()
    expect_null(bulk$data$id)
    parent$record(customer = "explicit")$create(flush_record = TRUE)
  }, .autoflush = FALSE)
  expect_false(is.null(explicit$data$id))

  # Flushing never commits on its own.
  tryCatch(
    with(engine, {
      parent$record(customer = "rolled back")$create()
      stop("abort")
    }, .autoflush = TRUE),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  expect_equal(
    nrow(parent$read(customer == "rolled back", .mode = "data.frame")),
    0L
  )
})

test_that("SQLite: an engine built with .autoflush = TRUE needs no per-block flag", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE,
    .autoflush = TRUE
  )
  withr::defer(engine$close())

  parent <- engine$model(
    "flush_orders2",
    id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
    customer = Column("TEXT")
  )
  child <- engine$model(
    "flush_order_items2",
    id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
    order_id = Column("INTEGER"),
    sku = Column("TEXT")
  )
  parent$create_table()
  child$create_table()

  item <- with(engine, {
    order <- parent$record(customer = "Alice")$create()
    expect_false(is.null(order$data$id))
    child$record(order_id = order$data$id, sku = "A-1")$create()
  })
  expect_equal(
    item$data$order_id,
    parent$read(customer == "Alice", .mode = "data.frame")$id
  )

  bulk <- with(engine, {
    parent$record(customer = "bulk")$create()
  }, .autoflush = FALSE)
  expect_null(bulk$data$id)
  expect_true(engine$get_autoflush())
})
