# Helper functions for Microsoft SQL Server tests.
#
# Mirrors helper-postgres.R: tests run against a throwaway docker container and
# skip whenever docker, the odbc package, or an ODBC driver for SQL Server is
# missing. Requires the msodbcsql18 driver on the host.

MSSQL_TEST_CONTAINER <- "orm_mssql_test"
MSSQL_TEST_IMAGE <- "mcr.microsoft.com/mssql/server:2022-latest"
MSSQL_TEST_PASSWORD <- "oRm_Test_Passw0rd!"
MSSQL_TEST_DB <- "tests"

mssql_driver_name <- function() {
    if (!requireNamespace("odbc", quietly = TRUE)) {
        return(NULL)
    }
    drivers <- tryCatch(odbc::odbcListDrivers()$name, error = function(e) character(0))
    drivers <- unique(drivers)
    candidates <- drivers[grepl("SQL Server", drivers, ignore.case = TRUE)]
    if (length(candidates) == 0) {
        return(NULL)
    }
    # Prefer the newest numbered Microsoft driver when several are installed.
    candidates[order(candidates, decreasing = TRUE)][1]
}

mssql_conn_args <- function(dbname = MSSQL_TEST_DB, driver = mssql_driver_name()) {
    list(
        drv = odbc::odbc(),
        Driver = driver,
        Server = "localhost,1433",
        Database = dbname,
        UID = "sa",
        PWD = MSSQL_TEST_PASSWORD,
        TrustServerCertificate = "yes",
        Encrypt = "no"
    )
}

check_for_mssql_test_db <- function(dbname = MSSQL_TEST_DB) {
    driver <- mssql_driver_name()
    if (is.null(driver)) {
        return(FALSE)
    }
    tryCatch({
        con <- do.call(DBI::dbConnect, mssql_conn_args(dbname, driver))
        on.exit(DBI::dbDisconnect(con), add = TRUE)
        DBI::dbGetQuery(con, "SELECT 1")
        TRUE
    }, error = function(e) FALSE)
}

setup_mssql_test_db <- function() {
    testthat::skip_on_cran()
    testthat::skip_if_not_installed("odbc")

    if (!docker_available()) {
        testthat::skip("Docker not available, skipping SQL Server tests")
    }
    if (is.null(mssql_driver_name())) {
        testthat::skip("No ODBC driver for SQL Server installed (msodbcsql18)")
    }

    # Remove any container left over from a previous run.
    tryCatch({
        system2("docker", args = c("rm", "-f", MSSQL_TEST_CONTAINER),
                stdout = FALSE, stderr = FALSE)
        Sys.sleep(1)
    }, error = function(e) invisible(NULL))

    message("Pulling SQL Server image (this may take a few minutes on first run)...")
    system2("docker", args = c("pull", MSSQL_TEST_IMAGE))

    message("Creating new SQL Server container...")
    system2("docker", args = c(
        "run", "-d",
        "--name", MSSQL_TEST_CONTAINER,
        "-e", "ACCEPT_EULA=Y",
        "-e", paste0("MSSQL_SA_PASSWORD=", MSSQL_TEST_PASSWORD),
        "-e", "MSSQL_PID=Developer",
        "-p", "1433:1433",
        MSSQL_TEST_IMAGE
    ))

    # SQL Server takes appreciably longer to accept connections than postgres.
    max_attempts <- 40
    ready <- FALSE
    for (i in seq_len(max_attempts)) {
        if (check_for_mssql_test_db("master")) {
            ready <- TRUE
            break
        }
        Sys.sleep(3)
    }
    if (!ready) {
        stop("SQL Server failed to start after ", max_attempts, " attempts")
    }

    # The image ships without our test database; create it once.
    con <- do.call(DBI::dbConnect, mssql_conn_args("master"))
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbExecute(con, paste0(
        "IF DB_ID(N'", MSSQL_TEST_DB, "') IS NULL CREATE DATABASE [", MSSQL_TEST_DB, "]"
    ))
    message("SQL Server container is ready!")

    mssql_conn_args()
}

use_mssql_test_db <- function() {
    testthat::skip_if_not_installed("odbc")
    if (!docker_available()) {
        testthat::skip("Docker not available for SQL Server tests")
    }
    if (is.null(mssql_driver_name())) {
        testthat::skip("No ODBC driver for SQL Server installed (msodbcsql18)")
    }

    if (!check_for_mssql_test_db()) {
        return(tryCatch(
            setup_mssql_test_db(),
            error = function(e) {
                testthat::skip(paste("Could not set up SQL Server test database:", e$message))
            }
        ))
    }

    mssql_conn_args()
}

#' An Engine pointed at the test container.
#'
#' Deliberately omits `.dialect` so that automatic detection from the ODBC
#' `Driver` argument is exercised by every integration test.
mssql_test_engine <- function(...) {
    conn_args <- use_mssql_test_db()
    do.call(Engine$new, c(conn_args, list(...)))
}

clear_mssql_test_tables <- function() {
    if (!docker_available() || is.null(mssql_driver_name())) {
        return(invisible(NULL))
    }

    tryCatch({
        con <- do.call(DBI::dbConnect, mssql_conn_args())
        on.exit(DBI::dbDisconnect(con), add = TRUE)

        # Foreign keys first, so tables can be dropped in any order.
        fks <- DBI::dbGetQuery(con, paste0(
            "SELECT s.name AS schema_name, t.name AS table_name, fk.name AS fk_name ",
            "FROM sys.foreign_keys fk ",
            "JOIN sys.tables t ON t.object_id = fk.parent_object_id ",
            "JOIN sys.schemas s ON s.schema_id = t.schema_id"
        ))
        for (i in seq_len(NROW(fks))) {
            DBI::dbExecute(con, paste0(
                "ALTER TABLE ", DBI::dbQuoteIdentifier(con, fks$schema_name[i]), ".",
                DBI::dbQuoteIdentifier(con, fks$table_name[i]),
                " DROP CONSTRAINT ", DBI::dbQuoteIdentifier(con, fks$fk_name[i])
            ))
        }

        tables <- DBI::dbGetQuery(con, paste0(
            "SELECT s.name AS schema_name, t.name AS table_name ",
            "FROM sys.tables t JOIN sys.schemas s ON s.schema_id = t.schema_id"
        ))
        for (i in seq_len(NROW(tables))) {
            DBI::dbExecute(con, paste0(
                "DROP TABLE IF EXISTS ",
                DBI::dbQuoteIdentifier(con, tables$schema_name[i]), ".",
                DBI::dbQuoteIdentifier(con, tables$table_name[i])
            ))
        }

        # Drop test schemas, leaving the built-ins in place.
        schemas <- DBI::dbGetQuery(con, paste0(
            "SELECT name FROM sys.schemas WHERE schema_id > 4 AND name NOT IN ",
            "('dbo','guest','INFORMATION_SCHEMA','sys','db_owner','db_accessadmin',",
            "'db_securityadmin','db_ddladmin','db_backupoperator','db_datareader',",
            "'db_datawriter','db_denydatareader','db_denydatawriter')"
        ))$name
        for (schema in schemas) {
            try(DBI::dbExecute(con, paste0(
                "DROP SCHEMA ", DBI::dbQuoteIdentifier(con, schema)
            )), silent = TRUE)
        }

        if (NROW(tables) > 0) {
            message("Cleared ", NROW(tables), " SQL Server test tables")
        }
    }, error = function(e) {
        message("Note: Could not clear SQL Server test tables: ", e$message)
    })
}

cleanup_mssql_test_db <- function() {
    if (!docker_available()) {
        return(invisible(NULL))
    }
    tryCatch({
        existing <- system2("docker",
            args = c("ps", "-a", "--filter", paste0("name=", MSSQL_TEST_CONTAINER),
                     "--format", "{{.Names}}"),
            stdout = TRUE, stderr = FALSE)
        if (length(existing) > 0 && nzchar(existing[1])) {
            message("Removing SQL Server container: ", MSSQL_TEST_CONTAINER)
            system2("docker", args = c("stop", "-t", "5", MSSQL_TEST_CONTAINER),
                    stdout = FALSE, stderr = FALSE)
            system2("docker", args = c("rm", "-f", MSSQL_TEST_CONTAINER),
                    stdout = FALSE, stderr = FALSE)
        } else {
            message("No SQL Server test containers found to clean up")
        }
    }, error = function(e) {
        message("Note: Could not access Docker containers for cleanup: ", e$message)
    })
}

reg.finalizer(environment(), function(e) {
    cleanup_mssql_test_db()
}, onexit = TRUE)
