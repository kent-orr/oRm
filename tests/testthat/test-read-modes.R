test_that("TableModel$read() handles all modes", {
    engine <- Engine$new(
        drv = RSQLite::SQLite(),
        dbname = ":memory:",
        persist = TRUE
    )

    user <- engine$model(
        "users",
        id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE)
    )

    user$create_table()

    user$record(id = 1, name = "Alice")$create()
    user$record(id = 2, name = "Bob")$create()

    all_records <- user$read(.mode = "all")
    expect_type(all_records, "list")
    expect_equal(length(all_records), 2)
    expect_true(all(vapply(all_records, inherits, logical(1), "Record")))

    single_record <- user$read(id == 1, .mode = "get")
    expect_true(inherits(single_record, "Record"))
    expect_equal(single_record$data$name, "Alice")

    expect_error(user$read(id == 3, .mode = "get"), "Expected exactly one row")

    optional_record <- user$read(id == 2, .mode = "one_or_none")
    expect_true(inherits(optional_record, "Record"))
    expect_equal(optional_record$data$name, "Bob")

    no_record <- user$read(id == 3, .mode = "one_or_none")
    expect_null(no_record)

    df <- user$read(.mode = "data.frame")
    expect_true(inherits(df, "data.frame"))
    expect_equal(nrow(df), 2)

    tbl_obj <- user$read(.mode = "tbl")
    expect_true(inherits(tbl_obj, "tbl"))
    expect_equal(nrow(dplyr::collect(tbl_obj)), 2)

    engine$close()
})


test_that("TableModel$read() handles unnamed arguments (logical expressions)", {
    engine <- Engine$new(
        drv = RSQLite::SQLite(),
        dbname = ":memory:",
        persist = TRUE
    )

    user <- engine$model(
        "users",
        id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE)
    )

    user$create_table()
    user$record(id = 1, name = "John")$create()
    user$record(id = 2, name = "Jane")$create()

    # TRUE should return all records (valid logical expression)
    result <- user$read(TRUE)
    expect_type(result, "list")
    expect_equal(length(result), 2)

    # FALSE should return no records
    result <- user$read(FALSE)
    expect_type(result, "list")
    expect_equal(length(result), 0)

    engine$close()
})

test_that("TableModel$read() handles expression evaluation", {
    engine <- Engine$new(
        drv = RSQLite::SQLite(),
        dbname = ":memory:",
        persist = TRUE
    )

    user <- engine$model(
        "users",
        id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE)
    )

    user$create_table()
    user$record(id = 1, name = "John")$create()
    user$record(id = 2, name = "Jane")$create()

    # Expression evaluation with column references
    result <- user$read(name == "John")
    expect_type(result, "list")
    expect_equal(length(result), 1)
    expect_equal(result[[1]]$data$name, "John")

    # Multiple expressions
    result <- user$read(id > 1, name == "Jane")
    expect_type(result, "list")
    expect_equal(length(result), 1)
    expect_equal(result[[1]]$data$name, "Jane")

    engine$close()
})

test_that(".mode='get' handles edge cases", {
    engine <- Engine$new(
        drv = RSQLite::SQLite(),
        dbname = ":memory:",
        persist = TRUE
    )

    user <- engine$model(
        "users",
        id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE)
    )

    user$create_table()

    expect_error(user$read(.mode = "get"), "Expected exactly one row")

    user$record(id = 1, name = "John")$create()
    user$record(id = 2, name = "Jane")$create()

    expect_error(user$read(.mode = "get"), "Expected exactly one row")

    engine$close()
})

test_that(".mode='one_or_none' handles edge cases", {
    engine <- Engine$new(
        drv = RSQLite::SQLite(),
        dbname = ":memory:",
        persist = TRUE
    )

    user <- engine$model(
        "users",
        id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE)
    )

    user$create_table(overwrite = T)

    result <- user$read(.mode = "one_or_none")
    expect_null(result)

    user$record(id = 1, name = "John")$create()
    user$record(id = 2, name = "Jane")$create()

    expect_error(user$read(.mode = "one_or_none"), "Expected zero or one row, got multiple")

    engine$close()
})

test_that(".mode='all' handles edge cases", {
    engine <- Engine$new(
        drv = RSQLite::SQLite(),
        dbname = ":memory:",
        persist = TRUE
    )

    user <- engine$model(
        "users",
        id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE)
    )

    user$create_table()

    result <- user$read(.mode = "all")
    expect_type(result, "list")
    expect_equal(length(result), 0)

    engine$close()
})

test_that(".mode='data.frame' handles empty query", {
    engine <- Engine$new(
        drv = RSQLite::SQLite(),
        dbname = ":memory:",
        persist = TRUE
    )

    user <- engine$model(
        "users",
        id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE)
    )

    user$create_table()

    result <- user$read(.mode = "data.frame")
    expect_true(inherits(result, "data.frame"))
    expect_equal(nrow(result), 0)

    engine$close()
})

test_that(".mode='tbl' handles empty query", {
    engine <- Engine$new(
        drv = RSQLite::SQLite(),
        dbname = ":memory:",
        persist = TRUE
    )

    user <- engine$model(
        "users",
        id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE)
    )

    user$create_table()

    result <- user$read(.mode = "tbl")
    expect_true(inherits(result, "tbl"))
    expect_equal(nrow(dplyr::collect(result)), 0)

    engine$close()
})

test_that("$get, $one_or_none, $all methods work equivalently", {
    engine <- Engine$new(
        drv = RSQLite::SQLite(),
        dbname = ":memory:",
        persist = TRUE
    )

    user <- engine$model(
        "users",
        id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE)
    )

    user$create_table()
    user$record(id = 1, name = "John")$create()
    user$record(id = 2, name = "Jane")$create()

    get_result <- user$get(id == 1)
    read_get_result <- user$read(id == 1, .mode = "get")
    expect_equal(get_result$data, read_get_result$data)

    one_or_none_result <- user$one_or_none(id == 1)
    read_one_or_none_result <- user$read(id == 1, .mode = "one_or_none")
    expect_equal(one_or_none_result$data, read_one_or_none_result$data)

    all_result <- user$all()
    read_all_result <- user$read(.mode = "all")
    expect_equal(length(all_result), length(read_all_result))

    engine$close()
})

test_that("$get, $one_or_none, $all methods error with .mode parameter", {
    engine <- Engine$new(
        drv = RSQLite::SQLite(),
        dbname = ":memory:",
        persist = TRUE
    )

    user <- engine$model(
        "users",
        id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE)
    )

    user$create_table()

    expect_error(user$get(.mode = "all"), 'formal argument ".mode" matched by multiple actual arguments')
    expect_error(user$one_or_none(.mode = "get"), 'formal argument ".mode" matched by multiple actual arguments')
    expect_error(user$all(.mode = "data.frame"), 'formal argument ".mode" matched by multiple actual arguments')

    engine$close()
})