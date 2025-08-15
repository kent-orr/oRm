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
