# Live integration tests for the ellmer database-agent (R/ellmer-agent.R).
#
# These drive a real LLM through register_db_tools() to confirm the registered
# CRUD tools actually round-trip: the model reads what the engine holds and its
# writes land in the database. They are slow and require a running Ollama with
# the model below, so they skip cleanly when that environment is absent. Set
# ORM_OLLAMA_MODEL to point at a different local model.

ollama_base_url <- function() {
    Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434")
}

ollama_model <- function() {
    Sys.getenv("ORM_OLLAMA_MODEL", "gemma4:31b")
}

# Skip unless a reachable Ollama is serving the requested model. We probe the
# /api/tags endpoint directly so the skip reason names the missing piece.
skip_if_no_ollama <- function(model = ollama_model()) {
    skip_if_not_installed("ellmer")
    skip_if_not_installed("httr2")

    tags <- tryCatch(
        httr2::request(ollama_base_url()) |>
            httr2::req_url_path("/api/tags") |>
            httr2::req_timeout(2) |>
            httr2::req_perform() |>
            httr2::resp_body_json(),
        error = function(e) skip(paste("Ollama not reachable:", conditionMessage(e)))
    )

    available <- vapply(tags$models, function(m) m$name, character(1))
    if (!model %in% available) {
        skip(sprintf("Ollama model '%s' not installed (have: %s)", model, paste(available, collapse = ", ")))
    }
}

# A fresh in-memory engine + seeded users model for each integration test.
seed_user_chat <- function() {
    engine <- Engine$new(drv = RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
    user <- engine$model(
        "users",
        id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE),
        age = Column("INTEGER")
    )
    user$create_table()
    user$record(id = 1, name = "Alice", age = 40)$create()
    user$record(id = 2, name = "Bob", age = 25)$create()

    chat <- ellmer::chat_ollama(
        model = ollama_model(),
        echo = "none",
        system_prompt = paste(
            "You are a database assistant. Use the provided tools to read and",
            "modify the database. Do not invent data; rely only on tool results."
        )
    )
    register_db_tools(chat, user)
    list(engine = engine, user = user, chat = chat)
}

test_that("agent reads through db_read and reports only matching rows", {
    skip_if_no_ollama()
    setup <- seed_user_chat()
    on.exit(setup$engine$close())

    answer <- setup$chat$chat(
        "Which users are older than 30? Reply with only their names."
    )

    expect_match(answer, "Alice", ignore.case = TRUE)
    expect_no_match(answer, "Bob", ignore.case = TRUE)
})

test_that("agent creates a row through db_create and it lands in the database", {
    skip_if_no_ollama()
    setup <- seed_user_chat()
    on.exit(setup$engine$close())

    setup$chat$chat("Add a user named Kent who is 35 years old.")

    kent <- setup$user$read(name == "Kent", .mode = "one_or_none")
    expect_false(is.null(kent))
    expect_equal(kent$data$age, 35L)
})

test_that("agent updates a row through db_update", {
    skip_if_no_ollama()
    setup <- seed_user_chat()
    on.exit(setup$engine$close())

    setup$chat$chat("Bob just had a birthday; set his age to 26.")

    bob <- setup$user$read(name == "Bob", .mode = "get")
    expect_equal(bob$data$age, 26L)
})

test_that("agent deletes a row through db_delete", {
    skip_if_no_ollama()
    setup <- seed_user_chat()
    on.exit(setup$engine$close())

    setup$chat$chat("Delete the user named Bob.")

    expect_null(setup$user$read(name == "Bob", .mode = "one_or_none"))
})

test_that("a read-only engine blocks agent writes", {
    skip_if_no_ollama()

    # Seed with a writable engine, then reopen the same database read-only and
    # expose that to the agent: db_create must surface the engine's refusal
    # rather than mutate the table.
    path <- tempfile(fileext = ".sqlite")
    on.exit(unlink(path), add = TRUE)

    writer <- Engine$new(drv = RSQLite::SQLite(), dbname = path, persist = TRUE)
    w_user <- writer$model(
        "users",
        id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE),
        age = Column("INTEGER")
    )
    w_user$create_table()
    w_user$record(id = 1, name = "Alice", age = 40)$create()
    writer$close()

    reader <- Engine$new(drv = RSQLite::SQLite(), dbname = path, persist = TRUE, .read_only = TRUE)
    on.exit(reader$close(), add = TRUE)
    ro_user <- reader$model(
        "users",
        id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE),
        age = Column("INTEGER")
    )

    chat <- ellmer::chat_ollama(
        model = ollama_model(),
        echo = "none",
        system_prompt = "You are a database assistant. Use the provided tools."
    )
    register_db_tools(chat, ro_user)
    chat$chat("Add a user named Kent who is 35 years old.")

    # The write must not have happened regardless of what the model claims.
    expect_equal(nrow(ro_user$read(.mode = "data.frame")), 1L)
    expect_null(ro_user$read(name == "Kent", .mode = "one_or_none"))
})
