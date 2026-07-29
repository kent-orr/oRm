# Tests for the optional ellmer database-agent integration (R/ellmer-agent.R).
# The pure translation/helper layer is tested without ellmer; tests that need a
# Chat object are skipped when ellmer is not installed.

make_user_model <- function() {
    engine <- Engine$new(
        drv = RSQLite::SQLite(),
        dbname = ":memory:",
        persist = TRUE
    )
    user <- engine$model(
        "users",
        id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE),
        age = Column("INTEGER")
    )
    user$create_table()
    user$record(id = 1, name = "Alice", age = 40)$create()
    user$record(id = 2, name = "Bob", age = 25)$create()
    list(engine = engine, user = user)
}

test_that("build_model_registry keys models by bare table name", {
    setup <- make_user_model()
    on.exit(setup$engine$close())

    registry <- build_model_registry(setup$user)
    expect_named(registry, "users")
    expect_true(inherits(registry$users, "TableModel"))
})

test_that("build_model_registry rejects non-model inputs", {
    expect_error(build_model_registry(42), "TableModel objects and/or an Engine")
})

test_that("schema_description enumerates tables and columns", {
    setup <- make_user_model()
    on.exit(setup$engine$close())

    desc <- schema_description(build_model_registry(setup$user))
    expect_match(desc, "users(", fixed = TRUE)
    expect_match(desc, "name TEXT", fixed = TRUE)
    expect_match(desc, "age INTEGER", fixed = TRUE)
})

test_that("get_model errors helpfully on unknown table", {
    setup <- make_user_model()
    on.exit(setup$engine$close())

    registry <- build_model_registry(setup$user)
    expect_error(get_model(registry, "ghosts"), "Unknown table 'ghosts'")
})

test_that("parse_values parses JSON objects and rejects empty input", {
    expect_equal(parse_values('{"name":"Kent","age":35}'), list(name = "Kent", age = 35L))
    expect_error(parse_values(""), "must be a JSON object")
    expect_error(parse_values(NULL), "must be a JSON object")
})

test_that("parse_filter yields a quosure that drives model$read", {
    setup <- make_user_model()
    on.exit(setup$engine$close())

    q <- parse_filter("age > 30")
    rows <- setup$user$read(!!q, .mode = "data.frame")
    expect_equal(nrow(rows), 1)
    expect_equal(rows$name, "Alice")
})

test_that("parse_filter cannot reach the caller's workspace", {
    sentinel <- "leaked"
    # `sentinel` lives in this test env, not in the filter's minimal env, so the
    # expression must not resolve it.
    q <- parse_filter("sentinel")
    expect_error(rlang::eval_tidy(q), "sentinel")
})

test_that("parse_filter drives model$update and model$delete", {
    setup <- make_user_model()
    on.exit(setup$engine$close())

    setup$user$update(!!parse_filter("id == 1"), .data = list(age = 41))
    expect_equal(setup$user$read(id == 1, .mode = "get")$data$age, 41L)

    setup$user$delete(!!parse_filter("id == 2"))
    expect_null(setup$user$read(id == 2, .mode = "one_or_none"))
})

test_that("register_db_tools registers the four CRUD tools", {
    skip_if_not_installed("ellmer")
    setup <- make_user_model()
    on.exit(setup$engine$close())

    chat <- tryCatch(
        ellmer::chat_openai(api_key = "test-key-not-used", model = "gpt-4o"),
        error = function(e) skip(paste("could not construct Chat:", conditionMessage(e)))
    )

    register_db_tools(chat, setup$user)
    tool_names <- names(chat$get_tools())
    expect_setequal(tool_names, c("db_read", "db_create", "db_update", "db_delete"))
})
