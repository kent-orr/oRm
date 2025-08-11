# Setup PostgreSQL container for testing
setup_postgres_test_db <- function() {
    if (!requireNamespace("stevedore", quietly = TRUE)) {
        install.packages("stevedore")
        if (!requireNamespace("stevedore", quietly = TRUE)) {
            testthat::skip("stevedore package not available, skipping PostgreSQL tests")
        }
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


test_that("engine schema operations work with Postgres", {
    if (!requireNamespace("RPostgres", quietly = TRUE)) {
        skip("RPostgres not available, skipping PostgreSQL tests")
    }

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

    engine$set_schema("public")
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

