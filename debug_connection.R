#!/usr/bin/env Rscript

# Debug connection validity issue
library(devtools)
load_all()

# Setup postgres test DB (skip CRAN check temporarily)
conn_info <- tryCatch({
    source("tests/testthat/helper-postgres.R")
    # Temporarily disable CRAN check
    assign("testthat_skip_on_cran", function() {}, envir = globalenv())
    environment(setup_postgres_test_db)$`testthat::skip_on_cran` <- function() {}
    
    setup_postgres_test_db()
}, error = function(e) {
    cat("Error setting up postgres:", e$message, "\n")
    quit(status = 1)
})

cat("Connection info:\n")
print(conn_info)

# Create engine
cat("\nCreating engine...\n")
engine <- Engine$new(conn_args = conn_info)
cat("Engine dialect:", engine$dialect, "\n")
cat("Engine schema:", engine$schema, "\n")

# Test connection
cat("\nTesting connection...\n")
conn1 <- engine$get_connection()
cat("Connection 1 valid:", DBI::dbIsValid(conn1), "\n")
cat("Connection 1 class:", class(conn1), "\n")

# Test again
conn2 <- engine$get_connection()
cat("Connection 2 valid:", DBI::dbIsValid(conn2), "\n")
cat("Same connection object:", identical(conn1, conn2), "\n")

# Try to use connection
cat("\nTesting connection usage...\n")
tryCatch({
    result <- DBI::dbQuoteIdentifier(conn1, "test_field")
    cat("dbQuoteIdentifier result:", result, "\n")
}, error = function(e) {
    cat("dbQuoteIdentifier error:", e$message, "\n")
})

# Try creating model and table
cat("\nTesting model creation...\n")
tryCatch({
    TempUser <- engine$model(
        "temp_users",
        id = Column("SERIAL", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE)
    )
    cat("Model created successfully\n")
    
    cat("Testing table creation...\n")
    TempUser$create_table(overwrite = TRUE)
    cat("Table created successfully\n")
}, error = function(e) {
    cat("Error:", e$message, "\n")
    cat("Call stack:\n")
    print(traceback())
})

# Cleanup
cat("\nCleaning up...\n")
source("tests/testthat/helper-postgres.R")
cleanup_postgres_test_db()