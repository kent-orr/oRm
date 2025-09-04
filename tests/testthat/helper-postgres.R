# Helper functions for PostgreSQL tests

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
    testthat::skip_if_not_installed("stevedore")
    testthat::skip_if_not_installed("RPostgres")

    if (!stevedore::docker_available()) {
        testthat::skip("Docker not available, skipping PostgreSQL tests")
    }

    # Clean up any existing postgres containers
    docker <- stevedore::docker_client()
    container_name <- "orm_postgres_test"
    
    # First, try to remove container by name (more reliable)
    tryCatch({
        existing_container <- docker$container$get(container_name)
        message("Stopping existing PostgreSQL container: ", container_name)
        existing_container$stop(t = 5)
        existing_container$remove(force = TRUE)
        Sys.sleep(1)  # Brief pause for cleanup
    }, error = function(e) invisible(NULL))
    
    # Also clean up any other postgres:14-alpine containers as backup
    tryCatch({
        containers <- docker$container$list(all = TRUE)
        for (container in containers) {
            if (grepl("postgres:14-alpine", container$image)) {
                message("Cleaning up PostgreSQL container: ", container$name)
                tryCatch({
                    container$stop(t = 5)
                    container$remove(force = TRUE)
                }, error = function(e) invisible(NULL))
            }
        }
    }, error = function(e) invisible(NULL))

    # Pull postgres image
    message("Pulling PostgreSQL Alpine image...")
    docker$image$pull("postgres:14-alpine")

    # Create and start new postgres container
    message("Creating new PostgreSQL container...")
    container <- docker$container$create(
        name = container_name,
        image = "postgres:14-alpine",
        env = c(
            POSTGRES_USER = "tester",
            POSTGRES_PASSWORD = "tester",
            POSTGRES_DB = "tests"
        ),
        ports = c("5432:5432")
    )
    container$start()

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
    if (!requireNamespace("stevedore", quietly = TRUE) || !stevedore::docker_available()) {
        testthat::skip("Docker not available for PostgreSQL tests")
    }
    
    # Check if test database is available
    if (!check_for_test_db()) {
        testthat::skip("PostgreSQL test database not available")
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
    if (!requireNamespace("stevedore", quietly = TRUE) || !stevedore::docker_available()) {
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
    if (!requireNamespace("stevedore", quietly = TRUE) || !stevedore::docker_available()) {
        return(invisible(NULL))
    }
    
    # Find and remove all postgres:14-alpine containers
    docker <- stevedore::docker_client()
    
    tryCatch({
        containers <- docker$container$list(all = TRUE)
        removed_count <- 0
        
        for (container in containers) {
            if (grepl("postgres:14-alpine", container$image)) {
                message("Removing PostgreSQL container: ", container$name)
                tryCatch({
                    container$stop(t = 5)
                    container$remove(force = TRUE)
                    removed_count <- removed_count + 1
                }, error = function(e) {
                    message("Warning: Could not remove container ", container$name, ": ", e$message)
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