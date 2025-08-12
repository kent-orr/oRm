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
  result = User$read(id == 1, mode='one_or_none')

  expect_true(inherits(result, "Record"))
  expect_equal(result$data$name, "Alice")

  # all: should return list of Record objects
  teens <- User$read(age >= 10, age < 20, mode = "all")
  expect_type(teens, "list")
  expect_true(all(vapply(teens, inherits, logical(1), "Record")))
  expect_equal(length(teens), 1)
  expect_equal(teens[[1]]$data$name, "Charlie")

  # get: fails if multiple rows match
  expect_error(
    User$read(age > 18, mode = "get"),
    "Expected exactly one row"
  )

  # one_or_none: returns NULL if no match
  none <- User$read(name == "Nobody", mode = "one_or_none")
  expect_null(none)

  # data.frame: returns collected rows
  df <- User$read(mode = "data.frame")
  expect_true(inherits(df, "data.frame"))
  expect_equal(nrow(df), 3)

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
  page1 <- Item$read(mode = "all", .limit = 5, .offset = 0)
  expect_equal(length(page1), 5)
  expect_equal(page1[[1]]$data$id, 1)
  expect_equal(page1[[5]]$data$id, 5)

  # Test case 2: Second page (items 6-10)
  page2 <- Item$read(mode = "all", .limit = 5, .offset = 5)
  expect_equal(length(page2), 5)
  expect_equal(page2[[1]]$data$id, 6)
  expect_equal(page2[[5]]$data$id, 10)

  # Test case 3: Last page with fewer items
  page4 <- Item$read(mode = "all", .limit = 5, .offset = 15)
  expect_equal(length(page4), 5)
  expect_equal(page4[[1]]$data$id, 16)
  expect_equal(page4[[5]]$data$id, 20)

  # Test case 4: Offset beyond available data
  empty_page <- Item$read(mode = "all", .limit = 5, .offset = 20)
  expect_null(empty_page)

  # Test case 5: Pagination with filtering
  filtered_page <- Item$read(position > 10, mode = "all", .limit = 5, .offset = 0)
  expect_equal(length(filtered_page), 5)
  expect_equal(filtered_page[[1]]$data$id, 11)
  expect_equal(filtered_page[[5]]$data$id, 15)

  # Test case 6: Pagination with filtering and offset
  filtered_page2 <- Item$read(position > 10, mode = "all", .limit = 5, .offset = 5)
  expect_equal(length(filtered_page2), 5)
  expect_equal(filtered_page2[[1]]$data$id, 16)
  expect_equal(filtered_page2[[5]]$data$id, 20)

  # Test case 7: Negative limit (last N items)
  last_items <- Item$read(mode = "all", .limit = -5)
  expect_equal(length(last_items), 5)
  expect_equal(last_items[[1]]$data$id, 16)
  expect_equal(last_items[[5]]$data$id, 20)

  # Clean up
  engine$close()
})
