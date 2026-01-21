# Flush data to database table

MySQL insert returning the last generated ID.

Insert a row and return the inserted record using PostgreSQL's RETURNING
clause.

This internal function inserts data into a database table using
dialect-specific logic for different database systems. It handles the
actual database write operation for Record objects.

SQLite implementation

## Usage

``` r
# S3 method for class 'mysql'
flush(x, table, data, con, commit = TRUE, ...)

# S3 method for class 'postgres'
flush(x, table, data, con, commit = TRUE, ...)

flush(x, table, data, con, commit = TRUE, ...)

# Default S3 method
flush(x, table, data, con, commit = TRUE, ...)

# S3 method for class 'sqlite'
flush(x, table, data, con, commit = TRUE, ...)
```

## Arguments

- x:

  An oRm object (Engine, TableModel, or Record) used for dialect
  dispatch

- table:

  Character string of the table name

- data:

  Named list or vector of data to insert

- con:

  DBI connection object

- commit:

  Logical indicating whether to commit the transaction

- ...:

  Additional arguments passed to dialect-specific methods

## Value

Invisible NULL or dialect-specific return value
