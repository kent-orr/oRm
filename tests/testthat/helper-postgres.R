# Helper functions for PostgreSQL tests

docker_available <- function() {
    # Check if docker command is available and working
    result <- tryCatch({
        system2("docker", args = "ps", stdout = FALSE, stderr = FALSE)
        TRUE
    }, error = function(e) {
        FALSE
    })
    return(result == TRUE || result == 0)
}

check_for_test_db <- function() {
    # attempt to connect to the tests database
    tryCatch({
        test_con <- DBI::dbConnect(
            RPostgres::Postgres(),
            dbname = "tests",
            host = "localhost",
            user = "tester",
            password = "tester",
            port = 5432,
            connect_timeout = 3
        )
        DBI::dbExecute(test_con, "SELECT 1")
        DBI::dbDisconnect(test_con)
        return(TRUE)
    }, error = function(e) {
        return(FALSE)
    })
}


setup_postgres_test_db <- function() {
    testthat::skip_on_cran()
    testthat::skip_if_not_installed("RPostgres")

    if (!docker_available()) {
        testthat::skip("Docker not available, skipping PostgreSQL tests")
    }

    container_name <- "orm_postgres_test"

    # Clean up any existing container by name
    tryCatch({
        message("Stopping existing PostgreSQL container: ", container_name)
        system2("docker", args = c("stop", container_name),
                stdout = FALSE, stderr = FALSE)
        system2("docker", args = c("rm", "-f", container_name),
                stdout = FALSE, stderr = FALSE)
        Sys.sleep(1)
    }, error = function(e) invisible(NULL))

    # Also clean up any other postgres:14-alpine containers
    tryCatch({
        containers <- system2("docker",
                            args = c("ps", "-a", "--filter", "ancestor=postgres:14-alpine",
                                   "--format", "{{.Names}}"),
                            stdout = TRUE, stderr = FALSE)
        if (length(containers) > 0 && nchar(containers[1]) > 0) {
            for (container in containers) {
                message("Cleaning up PostgreSQL container: ", container)
                system2("docker", args = c("stop", container),
                       stdout = FALSE, stderr = FALSE)
                system2("docker", args = c("rm", "-f", container),
                       stdout = FALSE, stderr = FALSE)
            }
        }
    }, error = function(e) invisible(NULL))

    # Pull postgres image
    message("Pulling PostgreSQL Alpine image...")
    system2("docker", args = c("pull", "postgres:14-alpine"))

    # Create and start new postgres container
    message("Creating new PostgreSQL container...")
    system2("docker", args = c(
        "run", "-d",
        "--name", container_name,
        "-e", "POSTGRES_USER=tester",
        "-e", "POSTGRES_PASSWORD=tester",
        "-e", "POSTGRES_DB=tests",
        "-p", "5432:5432",
        "postgres:14-alpine"
    ))

    # Wait for container to be ready
    max_attempts <- 15
    for (i in 1:max_attempts) {
        if (check_for_test_db()) {
            message("PostgreSQL container is ready!")
            break
        }
        if (i == max_attempts) {
            stop("PostgreSQL failed to start after ", max_attempts, " attempts")
        }
        Sys.sleep(2)
    }

    # if we're all good, return the connection details
    list(
        drv = RPostgres::Postgres(),
        dbname = "tests",
        host = "localhost",
        user = "tester",
        password = "tester",
        port = 5432
    )
}

use_postgres_test_db <- function() {
    if (!docker_available()) {
        testthat::skip("Docker not available for PostgreSQL tests")
    }

    # Check if test database is available
    if (!check_for_test_db()) {
        # If not available, try to set it up
        tryCatch({
            message("PostgreSQL test database not available, attempting to set up...")
            return(setup_postgres_test_db())
        }, error = function(e) {
            testthat::skip(paste("Could not set up PostgreSQL test database:", e$message))
        })
    }

    # Return connection parameters
    list(
        drv = RPostgres::Postgres(),
        dbname = "tests",
        host = "localhost",
        user = "tester",
        password = "tester",
        port = 5432
    )
}

clear_postgres_test_tables <- function() {
    if (!docker_available()) {
        return(invisible(NULL))
    }

    # Connect to the tests database
    tryCatch({
        con <- DBI::dbConnect(
            RPostgres::Postgres(),
            dbname = "tests",
            host = "localhost",
            user = "tester",
            password = "tester",
            port = 5432
        )

        # Get all user-created tables (excluding system tables)
        tables <- DBI::dbGetQuery(con, "
            SELECT table_name
            FROM information_schema.tables
            WHERE table_schema = 'public'
            AND table_type = 'BASE TABLE'
        ")

        # Drop each table
        if (nrow(tables) > 0) {
            for (table_name in tables$table_name) {
                DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ",
                                         DBI::dbQuoteIdentifier(con, table_name), " CASCADE"))
            }
            message("Cleared ", nrow(tables), " test tables")
        }

        DBI::dbDisconnect(con)
    }, error = function(e) {
        message("Note: Could not clear test tables: ", e$message)
    })
}

cleanup_postgres_test_db <- function() {
    if (!docker_available()) {
        return(invisible(NULL))
    }

    # Find and remove all postgres:14-alpine containers
    tryCatch({
        containers <- system2("docker",
                            args = c("ps", "-a", "--filter", "ancestor=postgres:14-alpine",
                                   "--format", "{{.Names}}"),
                            stdout = TRUE, stderr = FALSE)
        removed_count <- 0

        if (length(containers) > 0 && nchar(containers[1]) > 0) {
            for (container in containers) {
                message("Removing PostgreSQL container: ", container)
                tryCatch({
                    system2("docker", args = c("stop", "-t", "5", container),
                           stdout = FALSE, stderr = FALSE)
                    system2("docker", args = c("rm", "-f", container),
                           stdout = FALSE, stderr = FALSE)
                    removed_count <- removed_count + 1
                }, error = function(e) {
                    message("Warning: Could not remove container ", container, ": ", e$message)
                })
            }
        }

        if (removed_count > 0) {
            message("PostgreSQL test cleanup completed - removed ", removed_count, " containers")
        } else {
            message("No PostgreSQL test containers found to clean up")
        }

    }, error = function(e) {
        message("Note: Could not access Docker containers for cleanup: ", e$message)
    })
}

# Register cleanup on exit
reg.finalizer(environment(), function(e) {
    cleanup_postgres_test_db()
}, onexit = TRUE)