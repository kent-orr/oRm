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
    # Clean up any existing container
    tryCatch({
        existing_container <- docker$container$get(container_name)
        message("Stopping existing container...")
        existing_container$stop(t = 10)  # 10 second timeout
        existing_container$remove(force = TRUE)
        Sys.sleep(2)  # Brief pause after cleanup
    }, error = function(e) {
        # Container does not exist or already stopped
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
    
    # Health check with retry logic
    message("Waiting for PostgreSQL to start...")
    max_attempts <- 30
    attempt <- 1
    connected <- FALSE
    
    while (attempt <= max_attempts && !connected) {
        tryCatch({
            test_con <- DBI::dbConnect(
                RPostgres::Postgres(),
                dbname = "test",
                host = "localhost",
                user = "tester",
                password = "tester",
                port = 5432,
                connect_timeout = 5
            )
            DBI::dbExecute(test_con, "SELECT 1")
            DBI::dbDisconnect(test_con)
            connected <- TRUE
            message("PostgreSQL is ready!")
        }, error = function(e) {
            if (attempt == max_attempts) {
                stop("PostgreSQL failed to start after ", max_attempts, " attempts: ", e$message)
            }
            if (attempt %% 5 == 0) {
                message("Attempt ", attempt, "/", max_attempts, " - still waiting...")
            }
            Sys.sleep(2)
            attempt <<- attempt + 1
        })
    }

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
        # Force stop with timeout, then force remove
        container$stop(t = 5)
        Sys.sleep(1)
        container$remove(force = TRUE)
        message("PostgreSQL test container cleaned up")
    }, error = function(e) {
        # Try to remove any orphaned containers by name
        tryCatch({
            docker$container$remove(container_name, force = TRUE)
        }, error = function(e2) invisible(NULL))
    })
}

reg.finalizer(environment(), function(e) {
    cleanup_postgres_test_db()
}, onexit = TRUE)

