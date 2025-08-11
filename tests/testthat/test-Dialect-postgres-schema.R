# Setup PostgreSQL container for testing
setup_postgres_test_db <- function() {
    if (!requireNamespace("stevedore", quietly = TRUE)) {
        install.packages("stevedore")
        if (!requireNamespace("stevedore", quietly = TRUE)) {
            testthat::skip("stevedore package not available, skipping PostgreSQL tests")
        }
    }

    if (!requireNamespace("RPostgres", quietly = TRUE)) {
        skip("RPostgres not available, skipping PostgreSQL tests")
    }

    library(stevedore)
    if (!stevedore::docker_available()) {
        testthat::skip("Docker not available, skipping PostgreSQL tests")
    }

    docker <- stevedore::docker_client()
    container_name <- "orm_postgres_test"
    message("Pulling PostgreSQL Alpine image, this may take a while...")
    docker$image$pull("postgres:14-alpine")

    docker <- docker_client()
    container_name <- "orm_postgres_test"
    tryCatch({
        existing_container <- docker$container$get(container_name)
        message("Stopping existing container...")
        existing_container$stop()
        existing_container$remove(force = TRUE)
    }, error = function(e) {
        # container does not exist
    })

    container <- docker$container$create(
        name = container_name,
        image = "postgres:14-alpine",
        env = c(
            POSTGRES_USER = "tester",
            POSTGRES_PASSWORD = "tester",
            POSTGRES_DB = "test"
        ),
        ports = c("5432:5432")
    )
    container$start()
    message("Waiting for PostgreSQL to start...")
    Sys.sleep(5)

    list(
        drv = RPostgres::Postgres(),
        dbname = "test",
        host = "localhost",
        user = "tester",
        password = "tester",
        port = 5432
    )
}

cleanup_postgres_test_db <- function() {
    if (!requireNamespace("stevedore", quietly = TRUE) || !stevedore::docker_available()) {
        return(invisible(NULL))
    }
    docker <- stevedore::docker_client()
    container_name <- "orm_postgres_test"
    tryCatch({
        container <- docker$container$get(container_name)
        container$stop()
        container$remove()
    }, error = function(e) invisible(NULL))
}

reg.finalizer(environment(), function(e) {
    cleanup_postgres_test_db()
}, onexit = TRUE)

test_that('engine schema can be set on initialization', {
    conn_info <- tryCatch({
        c(setup_postgres_test_db(),.schema='test')
    }, error = function(e) {
        skip(paste("Could not set up PostgreSQL container:", e$message))
        NULL
    })
    if (is.null(conn_info)) {
        skip("PostgreSQL container setup failed")
    }

    engine <- do.call(Engine$new, conn_info)

    expect_equal(engine$schema, "test")
    expect_equal(engine$conn_args$dbname, "test")
    expect_equal(engine$conn_args$host, "localhost")
    expect_equal(engine$conn_args$user, "tester")

})

test_that('engine models create a schema if it does not exist', {

    conn_info <- tryCatch({
        c(setup_postgres_test_db(),.schema='test')
    }, error = function(e) {
        skip(paste("Could not set up PostgreSQL container:", e$message))
        NULL
    })
    if (is.null(conn_info)) {
        skip("PostgreSQL container setup failed")
    }

    engine <- do.call(Engine$new, conn_info)

    model = engine$model("users", id = Column("SERIAL", primary_key = TRUE), name = Column("TEXT", nullable = FALSE))
    model$create_table(overwrite = TRUE)
    expect_equal(model$tablename, "test.users")

    conn = engine$get_connection()
    res_schema = DBI::dbGetQuery(conn, "SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'test'")
    expect_equal(nrow(res_schema), 1)
    res_table = DBI::dbGetQuery(conn, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'test'")
    expect_true("users" %in% res_table$table_name)

    engine$execute("INSERT INTO test.users (name) VALUES ('Alice')")

    cleanup_postgres_test_db()


})

test_that('create_table creates schema when model schema changes', {
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        skip(paste("Could not set up PostgreSQL container:", e$message))
        NULL
    })
    if (is.null(conn_info)) {
        skip("PostgreSQL container setup failed")
    }

    engine <- do.call(Engine$new, conn_info)

    model <- engine$model("users", id = Column("SERIAL", primary_key = TRUE))
    model$set_schema("audit")
    model$create_table(overwrite = TRUE)

    conn <- engine$get_connection()
    res_schema <- DBI::dbGetQuery(conn, "SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'audit'")
    expect_equal(nrow(res_schema), 1)
    res_table <- DBI::dbGetQuery(conn, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'audit'")
    expect_true("users" %in% res_table$table_name)

    cleanup_postgres_test_db()
})

test_that("engine schema operations work with Postgres", {
    

    conn_info <- tryCatch({
        c(setup_postgres_test_db(), .schema='test')
    }, error = function(e) {
        skip(paste("Could not set up PostgreSQL container:", e$message))
        NULL
    })
    if (is.null(conn_info)) {
        skip("PostgreSQL container setup failed")
    }

    engine <- do.call(Engine$new, conn_info)

    UserPublic <- engine$model(
        "users",
        id = Column("SERIAL", primary_key = TRUE),
        name = Column("TEXT", nullable = FALSE)
    )
    UserPublic$create_table(overwrite = TRUE)
    expect_equal(UserPublic$tablename, "public.users")

    rec_public <- UserPublic$record(name = "Alice")
    rec_public$create()
    res_public <- UserPublic$read(mode = "all")
    expect_equal(length(res_public), 1)
    expect_equal(res_public[[1]]$data$name, "Alice")

    UserArchive <- engine$model("users", .schema = "archive")
    expect_equal(UserArchive$tablename, "archive.users")
    expect_equal(engine$schema, "public")

    DBI::dbExecute(engine$get_connection(), "CREATE SCHEMA IF NOT EXISTS audit")
    engine$set_schema("audit")
    engine$close()
    sp <- DBI::dbGetQuery(engine$get_connection(), "SHOW search_path")[[1]]
    expect_match(sp, '"audit"')
    UserArchive$set_schema(engine$schema)
    expect_equal(UserArchive$tablename, "audit.users")

    UserArchive$create_table(overwrite = TRUE)
    rec_audit <- UserArchive$record(name = "Bob")
    rec_audit$create()
    res_audit <- UserArchive$read(mode = "all")
    expect_equal(length(res_audit), 1)
    expect_equal(res_audit[[1]]$data$name, "Bob")

    engine$set_schema("public")
    rec_audit$set_schema(engine$schema)
    expect_equal(rec_audit$model$tablename, "public.users")

    UserArchive$drop_table(ask = FALSE)
    UserPublic$drop_table(ask = FALSE)
    engine$close()
})

