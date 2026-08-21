testthat::source_test_helpers()

# Composite (multi-column) primary keys. The query-side helpers (pk_fields,
# build_key_where) have always handled multiple keys; these tests pin down the
# DDL side, which must emit one table-level PRIMARY KEY constraint rather than
# an inline clause per column.

sqlite_engine <- function() {
  skip_if_not_installed("RSQLite")
  Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
}

test_that("single-column primary keys still render inline", {
  engine <- sqlite_engine()
  withr::defer(engine$close())

  model <- engine$model(
    "single_pk",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT")
  )

  sql <- model$create_table(verbose = TRUE)
  q <- function(x) DBI::dbQuoteIdentifier(engine$get_connection(), x)

  expect_match(sql, paste0(q("id"), " INTEGER PRIMARY KEY"), fixed = TRUE)
  # No table-level constraint for the single-key case.
  expect_false(grepl("PRIMARY KEY (", sql, fixed = TRUE))
})

test_that("composite primary keys render as one table-level constraint", {
  engine <- sqlite_engine()
  withr::defer(engine$close())

  model <- engine$model(
    "composite_pk",
    org_id = Column("INTEGER", primary_key = TRUE),
    user_id = Column("INTEGER", primary_key = TRUE),
    role = Column("TEXT")
  )

  sql <- model$create_table(verbose = TRUE)
  q <- function(x) DBI::dbQuoteIdentifier(engine$get_connection(), x)

  expect_match(
    sql,
    paste0("PRIMARY KEY (", q("org_id"), ", ", q("user_id"), ")"),
    fixed = TRUE
  )
  # Exactly one PRIMARY KEY clause in the whole statement.
  expect_equal(lengths(regmatches(sql, gregexpr("PRIMARY KEY", sql)))[[1]], 1L)
  # Inline clauses must be suppressed on the key columns.
  expect_false(grepl(paste0(q("org_id"), " INTEGER PRIMARY KEY"), sql, fixed = TRUE))
  expect_false(grepl(paste0(q("user_id"), " INTEGER PRIMARY KEY"), sql, fixed = TRUE))
  # Key columns are NOT NULL, as a primary key requires.
  expect_match(sql, paste0(q("org_id"), " INTEGER NOT NULL"), fixed = TRUE)
  expect_match(sql, paste0(q("user_id"), " INTEGER NOT NULL"), fixed = TRUE)
})

test_that("composite primary keys coexist with foreign key constraints", {
  engine <- sqlite_engine()
  withr::defer(engine$close())

  model <- engine$model(
    "memberships",
    org_id = Column("INTEGER", primary_key = TRUE),
    user_id = ForeignKey("INTEGER", references = "users.id", primary_key = TRUE),
    role = Column("TEXT")
  )

  sql <- model$create_table(verbose = TRUE)
  q <- function(x) DBI::dbQuoteIdentifier(engine$get_connection(), x)

  expect_match(
    sql,
    paste0("PRIMARY KEY (", q("org_id"), ", ", q("user_id"), ")"),
    fixed = TRUE
  )
  expect_match(
    sql,
    paste0(
      "FOREIGN KEY (", q("user_id"), ") REFERENCES ", q("users"), " (", q("id"), ")"
    ),
    fixed = TRUE
  )
})

test_that("composite primary key tables support full CRUD", {
  engine <- sqlite_engine()
  withr::defer(engine$close())

  model <- engine$model(
    "composite_crud",
    org_id = Column("INTEGER", primary_key = TRUE),
    user_id = Column("INTEGER", primary_key = TRUE),
    role = Column("TEXT")
  )
  model$create_table(overwrite = TRUE, ask = FALSE)

  model$record(org_id = 1, user_id = 1, role = "admin")$create()
  model$record(org_id = 1, user_id = 2, role = "member")$create()
  model$record(org_id = 2, user_id = 1, role = "member")$create()

  expect_equal(length(model$read(.mode = "all")), 3L)

  # An update must address exactly the row whose full key matches.
  rec <- model$read(org_id == 1, user_id == 2, .mode = "one")
  rec$data$role <- "owner"
  rec$update()

  expect_equal(model$read(org_id == 1, user_id == 2, .mode = "one")$data$role, "owner")
  expect_equal(model$read(org_id == 1, user_id == 1, .mode = "one")$data$role, "admin")
  expect_equal(model$read(org_id == 2, user_id == 1, .mode = "one")$data$role, "member")

  # Same for delete: only the fully-matching row goes away.
  model$read(org_id == 1, user_id == 2, .mode = "one")$delete()

  remaining <- model$read(.mode = "data.frame")
  expect_equal(nrow(remaining), 2L)
  expect_false(any(remaining$org_id == 1 & remaining$user_id == 2))
})

test_that("refresh reloads a composite-key record", {
  engine <- sqlite_engine()
  withr::defer(engine$close())

  model <- engine$model(
    "composite_refresh",
    org_id = Column("INTEGER", primary_key = TRUE),
    user_id = Column("INTEGER", primary_key = TRUE),
    role = Column("TEXT")
  )
  model$create_table(overwrite = TRUE, ask = FALSE)
  model$record(org_id = 7, user_id = 9, role = "member")$create()

  rec <- model$read(org_id == 7, user_id == 9, .mode = "one")
  rec$data$role <- "scribbled locally"
  rec$refresh()

  expect_equal(rec$data$role, "member")
})
