test_that("TableModel initializes and defines fields correctly", {

  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )

  model <- TableModel$new(
    tablename = "test_TableModel",
    engine = engine,
    id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
    name = Column("TEXT", nullable = FALSE),
    created_at = Column("TIMESTAMP")
  )

  # Check table name and engine binding
  expect_equal(model$tablename, "test_TableModel")
  expect_identical(model$engine, engine)

  # Check that all expected fields are present
  expect_setequal(names(model$fields), c("id", "name", "created_at"))

  
  # Create the table in the DB
  con <- model$get_connection()
  expect_true(DBI::dbIsValid(con))

  DBI::dbExecute(con, "DROP TABLE IF EXISTS test_TableModel")
  model$create_table(verbose = TRUE)
  model$create_table()
  expect_true("test_TableModel" %in% DBI::dbListTables(con))

  # Verify table structure
  table_info <- DBI::dbGetQuery(con, "PRAGMA table_info(test_TableModel)")
  expect_equal(nrow(table_info), 3)
  expect_equal(table_info$name, c("id", "name", "created_at"))
  expect_equal(table_info$type, c("INTEGER", "TEXT", "TIMESTAMP"))
  # note that primary keys drop nullable as that's handled by the sql server
  expect_equal(table_info$notnull, c(0, 1, 0))
  expect_equal(table_info$pk, c(1, 0, 0))
  # Print shouldn't error
  expect_no_error(print(model))

  # Clean up
  DBI::dbExecute(con, "DROP TABLE IF EXISTS test_TableModel")
  engine$close()
})

test_that("TableModel$create_table() respects if_not_exists and overwrite arguments", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )

  model <- TableModel$new(
    tablename = "test_create_options",
    engine = engine,
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT")
  )

  # Helper function to get table info
  get_table_info <- function() {
    DBI::dbGetQuery(engine$get_connection(), "PRAGMA table_info(test_create_options)")
  }

  # Test 1: Default behavior (if_not_exists = TRUE, overwrite = FALSE)
  model$create_table()
  expect_true("test_create_options" %in% DBI::dbListTables(engine$get_connection()))
  initial_info <- get_table_info()

  # Creating again shouldn't change anything
  model$create_table()
  expect_identical(get_table_info(), initial_info)

  # Test 2: Attempt to create without if_not_exists
  expect_error(model$create_table(if_not_exists = FALSE), "already exists")

  # Test 3: Overwrite existing table
  model_new <- TableModel$new(
    tablename = "test_create_options",
    engine = engine,
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT"),
    age = Column("INTEGER")  # New column
  )

  model_new$create_table(overwrite = TRUE)
  new_info <- get_table_info()
  expect_equal(nrow(new_info), 3)  # Should now have 3 columns
  expect_true("age" %in% new_info$name)

  # Test 4: Overwrite with if_not_exists
  model$create_table(overwrite = TRUE, if_not_exists = TRUE)
  expect_identical(get_table_info(), initial_info)  # Should be back to original structure

  # Test 5: Verbose output
  sql_output <- model$create_table(verbose = TRUE)
  expect_type(sql_output, "character")
  expect_true(grepl("CREATE TABLE IF NOT EXISTS", sql_output))

  # Clean up
  DBI::dbExecute(engine$get_connection(), "DROP TABLE IF EXISTS test_create_options")
  engine$close()
})



test_that("TableModel$read() works with filter expressions and mode", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )

  User <- engine$model(
    "users",
    id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
    name = Column("TEXT", nullable = FALSE),
    age = Column("INTEGER")
  )

  User$create_table()

  # Insert multiple users
  User$record(id=1, name = "Alice", age = 30)$create()
  User$record(id=2, name = "Bob", age = 25)$create()
  User$record(id=3, name = "Charlie", age = 17)$create()

  # one_or_none: should return one Record
  result = User$read(id == 1, .mode='one_or_none')

  expect_true(inherits(result, "Record"))
  expect_equal(result$data$name, "Alice")

  # all: should return list of Record objects
  teens <- User$read(age >= 10, age < 20, .mode = "all")
  expect_type(teens, "list")
  expect_true(all(vapply(teens, inherits, logical(1), "Record")))
  expect_equal(length(teens), 1)
  expect_equal(teens[[1]]$data$name, "Charlie")

  # get: fails if multiple rows match
  expect_error(
    User$read(age > 18, .mode = "get"),
    "Expected exactly one row"
  )

  # one_or_none: returns NULL if no match
  none <- User$read(name == "Nobody", .mode = "one_or_none")
  expect_null(none)

  # data.frame: returns collected rows
  df <- User$read(.mode = "data.frame")
  expect_true(inherits(df, "data.frame"))
  expect_equal(nrow(df), 3)

    # tbl: returns lazy table
    tbl_obj <- User$read(.mode = "tbl")
    expect_true(inherits(tbl_obj, "tbl"))
    filtered_tbl <- tbl_obj |> dplyr::filter(age > 20) |> dplyr::collect()
    expect_equal(nrow(filtered_tbl), 2)

  engine$close()
})

test_that("TableModel$read() supports pagination with limit and offset", {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )

  # Create a model for testing pagination
  Item <- engine$model(
    "items",
    id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
    name = Column("TEXT", nullable = FALSE),
    position = Column("INTEGER", nullable = FALSE)
  )

  Item$create_table()

  # Insert 20 items to test pagination
  for (i in 1:20) {
    Item$record(id = i, name = paste0("Item ", i), position = i)$create()
  }

  # Test case 1: Basic pagination - first page (items 1-5)
  page1 <- Item$read(.mode = "all", .limit = 5, .offset = 0)
  expect_equal(length(page1), 5)
  expect_equal(page1[[1]]$data$id, 1)
  expect_equal(page1[[5]]$data$id, 5)

  # Test case 2: Second page (items 6-10)
  page2 <- Item$read(.mode = "all", .limit = 5, .offset = 5)
  expect_equal(length(page2), 5)
  expect_equal(page2[[1]]$data$id, 6)
  expect_equal(page2[[5]]$data$id, 10)

  # Test case 3: Last page with fewer items
  page4 <- Item$read(.mode = "all", .limit = 5, .offset = 15)
  expect_equal(length(page4), 5)
  expect_equal(page4[[1]]$data$id, 16)
  expect_equal(page4[[5]]$data$id, 20)

  # Test case 4: Offset beyond available data
  empty_page <- Item$read(.mode = "all", .limit = 5, .offset = 20)
  expect_equal(empty_page, list())

  # Test case 5: Pagination with filtering
  filtered_page <- Item$read(position > 10, .mode = "all", .limit = 5, .offset = 0)
  expect_equal(length(filtered_page), 5)
  expect_equal(filtered_page[[1]]$data$id, 11)
  expect_equal(filtered_page[[5]]$data$id, 15)

  # Test case 6: Pagination with filtering and offset
  filtered_page2 <- Item$read(position > 10, .mode = "all", .limit = 5, .offset = 5)
  expect_equal(length(filtered_page2), 5)
  expect_equal(filtered_page2[[1]]$data$id, 16)
  expect_equal(filtered_page2[[5]]$data$id, 20)

  # Test case 7: Negative limit (last N items)
  last_items <- Item$read(.mode = "all", .limit = -5)
  expect_equal(length(last_items), 5)
  expect_equal(last_items[[1]]$data$id, 16)
  expect_equal(last_items[[5]]$data$id, 20)

  # Clean up
  engine$close()
})

test_that("create_table(overwrite=TRUE) prompts for confirmation in interactive sessions", {
  skip_if_not_installed("testthat", "3.5.0")

  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  on.exit(engine$close(), add = TRUE)

  Model <- TableModel$new(
    tablename = "confirm_overwrite",
    engine = engine,
    id   = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT")
  )
  Model$create_table()

  # Seed a row so we can detect whether the table was actually overwritten.
  Model$record(id = 1, name = "before")$create()
  expect_equal(Model$read(.mode = "data.frame")$name, "before")

  # Decline the prompt: error is raised and the table is left intact.
  testthat::local_mocked_bindings(
    interactive = function() TRUE,
    readline    = function(...) "n",
    .package = "oRm"
  )
  expect_error(
    Model$create_table(overwrite = TRUE),
    "did not confirm overwrite"
  )
  expect_equal(Model$read(.mode = "data.frame")$name, "before")
})

test_that("create_table(overwrite=TRUE) proceeds when user confirms", {
  skip_if_not_installed("testthat", "3.5.0")

  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  on.exit(engine$close(), add = TRUE)

  Model <- TableModel$new(
    tablename = "confirm_overwrite_yes",
    engine = engine,
    id   = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT")
  )
  Model$create_table()
  Model$record(id = 1, name = "before")$create()

  testthat::local_mocked_bindings(
    interactive = function() TRUE,
    readline    = function(...) "y",
    .package = "oRm"
  )
  expect_no_error(Model$create_table(overwrite = TRUE))

  # Table was dropped and recreated; the seeded row should be gone.
  expect_equal(nrow(Model$read(.mode = "data.frame")), 0)
})

test_that("create_table(overwrite=TRUE, ask=FALSE) bypasses the prompt", {
  skip_if_not_installed("testthat", "3.5.0")

  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  on.exit(engine$close(), add = TRUE)

  Model <- TableModel$new(
    tablename = "skip_prompt",
    engine = engine,
    id = Column("INTEGER", primary_key = TRUE)
  )
  Model$create_table()

  # Mock interactive() as TRUE to prove ask=FALSE is what skips the prompt,
  # not the non-interactive test session. readline() must never be called.
  readline_called <- FALSE
  testthat::local_mocked_bindings(
    interactive = function() TRUE,
    readline    = function(...) { readline_called <<- TRUE; "n" },
    .package = "oRm"
  )

  expect_no_error(Model$create_table(overwrite = TRUE, ask = FALSE))
  expect_false(readline_called)
})

# --- Set-level CRUD (TableModel$create/update/delete) ---------------------

# Build an engine + seeded model for the set-level CRUD tests.
setup_crud_model <- function(read_only = FALSE) {
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE,
    .read_only = read_only
  )
  Model <- engine$model(
    "test_set_crud",
    id   = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
    name = Column("TEXT"),
    age  = Column("INTEGER")
  )
  list(engine = engine, model = Model)
}

seed_rows <- function(Model) {
  Model$record(id = 1L, name = "Kent",  age = 34L)$create()
  Model$record(id = 2L, name = "Dylan", age = 25L)$create()
  Model$record(id = 3L, name = "Ada",   age = 36L)$create()
}

test_that("TableModel$create() inserts a row and returns a Record", {
  ctx <- setup_crud_model()
  on.exit(ctx$engine$close())
  ctx$model$create_table()

  rec <- ctx$model$create(id = 1L, name = "Kent", age = 34L)
  expect_s3_class(rec, "Record")
  expect_equal(rec$data$name, "Kent")

  # Parity with record(...)$create()
  rec2 <- ctx$model$record(id = 2L, name = "Dylan", age = 25L)$create()
  got <- ctx$model$read(.mode = "data.frame")
  expect_setequal(got$id, c(1L, 2L))
})

test_that("TableModel$update() applies NSE split (bare = WHERE, named = SET)", {
  ctx <- setup_crud_model()
  on.exit(ctx$engine$close())
  ctx$model$create_table()
  seed_rows(ctx$model)

  n <- ctx$model$update(id == 1, name = "Kent O", age = 35L)
  expect_equal(n, 1L)

  row1 <- ctx$model$read(id == 1, .mode = "data.frame")
  expect_equal(row1$name, "Kent O")
  expect_equal(row1$age, 35L)

  # Unaffected rows are untouched
  others <- ctx$model$read(id != 1, .mode = "data.frame")
  expect_setequal(others$name, c("Dylan", "Ada"))

  # `name == "x"` would be a filter, not a SET — prove the split:
  # set age on the row WHERE name == "Ada"
  n2 <- ctx$model$update(name == "Ada", age = 99L)
  expect_equal(n2, 1L)
  expect_equal(ctx$model$read(name == "Ada", .mode = "data.frame")$age, 99L)
})

test_that("TableModel$delete() removes only matching rows", {
  ctx <- setup_crud_model()
  on.exit(ctx$engine$close())
  ctx$model$create_table()
  seed_rows(ctx$model)

  n <- ctx$model$delete(age < 30)
  expect_equal(n, 1L)
  remaining <- ctx$model$read(.mode = "data.frame")
  expect_setequal(remaining$name, c("Kent", "Ada"))
})

test_that("filterless update/delete are guarded but .all = TRUE proceeds", {
  ctx <- setup_crud_model()
  on.exit(ctx$engine$close())
  ctx$model$create_table()
  seed_rows(ctx$model)

  expect_error(ctx$model$update(age = 0L), "Refusing to update all rows")
  expect_error(ctx$model$delete(), "Refusing to delete all rows")

  expect_equal(ctx$model$update(age = 0L, .all = TRUE), 3L)
  expect_true(all(ctx$model$read(.mode = "data.frame")$age == 0L))

  expect_equal(ctx$model$delete(.all = TRUE), 3L)
  expect_equal(nrow(ctx$model$read(.mode = "data.frame")), 0L)
})

test_that("update() requires at least one SET value", {
  ctx <- setup_crud_model()
  on.exit(ctx$engine$close())
  ctx$model$create_table()
  seed_rows(ctx$model)

  expect_error(ctx$model$update(id == 1), "No values to set")
})

test_that("set-level update/delete error without a primary key", {
  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  on.exit(engine$close())
  Model <- engine$model(
    "test_no_pk",
    name = Column("TEXT"),
    age  = Column("INTEGER")
  )
  Model$create_table()
  Model$record(name = "Kent", age = 34L)$create()

  expect_error(Model$update(name == "Kent", age = 1L), "primary key")
  expect_error(Model$delete(name == "Kent"), "primary key")
})

test_that("set-level writes are blocked on a read-only engine", {
  # Seed with a writable engine first, then reopen read-only on the same file.
  path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(path), add = TRUE)

  w <- Engine$new(drv = RSQLite::SQLite(), dbname = path, persist = TRUE)
  Wm <- w$model(
    "test_ro",
    id   = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT")
  )
  Wm$create_table()
  Wm$record(id = 1L, name = "Kent")$create()
  w$close()

  ro <- Engine$new(drv = RSQLite::SQLite(), dbname = path, persist = TRUE, .read_only = TRUE)
  on.exit(ro$close(), add = TRUE)
  Rm <- ro$model(
    "test_ro",
    id   = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT")
  )

  expect_error(Rm$create(id = 2L, name = "Ghost"), "read-only")
  expect_error(Rm$update(id == 1, name = "X"), "read-only")
  expect_error(Rm$delete(id == 1), "read-only")
})

# =============================================================================
# REVIEW BUGS: TableModel$read()/all() silent row cap and unquoted identifiers
# =============================================================================
# These tests assert the intended contract and currently fail, documenting bugs
# surfaced in review.

# REVIEW BUG: read()/all() silently cap at 100 rows (R/TableModel.R:419,598).
# A method literally named all() should return every matching row, not silently
# the first 100. (If the design intent is a hard cap, all() at minimum ought to
# warn on truncation rather than drop rows quietly.)
test_that("Model$all() returns every row, not a silent 100-row cap", {
  skip_if_not_installed("RSQLite")

  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  on.exit(engine$close(), add = TRUE)

  Item <- engine$model(
    "items_cap",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT")
  )
  Item$create_table(overwrite = TRUE, ask = FALSE)

  for (i in seq_len(150)) {
    Item$record(id = i, name = paste0("n", i))$create()
  }

  expect_equal(length(Item$all()), 150L)
})

# REVIEW BUG: column identifiers are interpolated bare in WHERE/SET clauses
# (R/Record.R:281-285,315-319; R/sql-helpers.R), while the table name is quoted
# via format_tablename(). A column whose name is a reserved word (e.g. "order")
# produces invalid SQL. They should be quoted with DBI::dbQuoteIdentifier.
test_that("update/delete handle a reserved-word column name", {
  skip_if_not_installed("RSQLite")

  engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
  on.exit(engine$close(), add = TRUE)

  Item <- engine$model(
    "items_reserved",
    id    = Column("INTEGER", primary_key = TRUE),
    order = Column("INTEGER")
  )
  Item$create_table(overwrite = TRUE, ask = FALSE)
  Item$record(id = 1L, order = 5L)$create()

  rec <- Item$get(id == 1L)

  # Row-level update of a reserved-word column.
  expect_no_error(rec$update(order = 10L))
  expect_equal(Item$get(id == 1L)$data$order, 10L)

  # Set-level update targeting the same column.
  expect_no_error(Item$update(id == 1L, order = 20L))
  expect_equal(Item$get(id == 1L)$data$order, 20L)

  # Row-level delete builds its WHERE from the (quoted) key column.
  expect_no_error(rec$delete())
  expect_null(Item$one_or_none(id == 1L))
})
