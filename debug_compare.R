#!/usr/bin/env Rscript

library(devtools)
load_all()
source("tests/testthat/helper-postgres.R")

# Method 1: Direct use_postgres_test_db (what works in my debug script)
cat("Method 1: use_postgres_test_db()\n")
conn_info1 <- use_postgres_test_db()
print(conn_info1)

# Method 2: What the test does (setup first)
cat("\nMethod 2: setup_postgres_test_db() then use_postgres_test_db()\n")
tryCatch({
    setup_conn_info <- setup_postgres_test_db()
    print(setup_conn_info)
    conn_info2 <- use_postgres_test_db()
    print(conn_info2)
}, error = function(e) {
    cat("Error in method 2:", e$message, "\n")
})

# Now test both with Engine creation and table creation
cat("\n=== Testing Method 1 ===\n")
engine1 <- Engine$new(conn_args = conn_info1)
cat("Engine1 created, dialect:", engine1$dialect, "\n")

TempUser1 <- engine1$model(
    "temp_users1",
    id = Column("SERIAL", primary_key = TRUE, nullable = FALSE),
    name = Column("TEXT", nullable = FALSE)
)

tryCatch({
    TempUser1$create_table(overwrite = TRUE)
    cat("Table creation successful with Method 1\n")
}, error = function(e) {
    cat("Table creation failed with Method 1:", e$message, "\n")
})