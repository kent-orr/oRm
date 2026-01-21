# Execute a SQL statement

This internal function executes SQL commands using dialect-specific
logic.

## Usage

``` r
execute_sql.postgres(x, con, sql)

execute_sql(x, con, sql)

execute_sql.default(x, con, sql)
```

## Arguments

- x:

  An oRm object (Engine, TableModel, or Record) used for dialect
  dispatch

- con:

  DBI connection object

- sql:

  Character string of the SQL statement to execute

## Functions

- `execute_sql.postgres()`: Suppress PostgreSQL messages when executing
  SQL commands.
