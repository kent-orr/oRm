# Detect whether a SQL string is a read-only statement.

Used to gate \`Engine\$execute()\` when the engine is configured as
read-only. Conservative: only matches statements that begin with one of
SELECT, WITH, EXPLAIN, SHOW, PRAGMA, VALUES (after stripping leading
whitespace and \`–\` line comments).

## Usage

``` r
is_read_sql(sql)
```

## Arguments

- sql:

  Character. SQL statement.

## Value

Logical.
