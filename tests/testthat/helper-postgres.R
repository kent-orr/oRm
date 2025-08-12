# Helper functions for PostgreSQL tests

setup_postgres_test_db <- function() {
    testthat::skip_on_cran()
    testthat::skip_if_not_installed("stevedore")
    testthat::skip_if_not_installed("RPostgres")

    if (!stevedore::docker_available()) {
        testthat::skip("Docker not available, skipping PostgreSQL tests")
    }

    docker <- stevedore::docker_client()
    container_name <- "orm_postgres_test"
    message("Pulling PostgreSQL Alpine image, this may take a while...")
    docker$image$pull("postgres:14-alpine")

    docker <- stevedore::docker_client()
    tryCatch({
        existing_container <- docker$container$get(container_name)
        message("Stopping existing container...")
        existing_container$stop()
        existing_container$remove(force = TRUE)
    }, error = function(e) {
        # Container does not exist
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

