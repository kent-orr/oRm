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

  all_records <- Example$read(mode = "all")
  expect_equal(length(all_records), 2L)
  expect_equal(all_records[[1]]$data$id, 1L)
  expect_equal(all_records[[1]]$data$name, "alpha")
  expect_equal(all_records[[2]]$data$id, 2L)
  expect_equal(all_records[[2]]$data$name, "anon")

  flush_res <- oRm:::flush(engine, Example$tablename, list(name = "beta"), engine$get_connection())
  expect_equal(flush_res$name, "beta")
  expect_equal(flush_res$id, 3L)

  Example$drop_table(ask = FALSE)
  engine$close()
})
