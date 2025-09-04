#!/usr/bin/env Rscript

# Simple debug of connection issue
library(devtools)
load_all()
source("tests/testthat/helper-postgres.R")

# Get connection info
conn_info <- use_postgres_test_db()
cat("Got connection info\n")

# Create engine without schema
engine <- Engine$new(conn_args = conn_info)
cat("Created engine with dialect:", engine$dialect, "and schema:", engine$schema, "\n")

# Get connection
conn <- engine$get_connection()
cat("Got connection, valid:", DBI::dbIsValid(conn), "\n")

# Test dbQuoteIdentifier directly
cat("Testing dbQuoteIdentifier...\n")
result <- DBI::dbQuoteIdentifier(conn, "test_field")
cat("Result:", result, "\n")

cat("Connection still valid:", DBI::dbIsValid(conn), "\n")