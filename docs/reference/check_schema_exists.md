# Check whether a schema exists for the current dialect

Dialects that do not implement schemas should return TRUE.

## Usage

``` r
check_schema_exists.mysql(x, .schema)

check_schema_exists.postgres(x, .schema)

check_schema_exists(x, .schema)

check_schema_exists.default(x, .schema)

check_schema_exists.sqlite(x, .schema)
```

## Arguments

- x:

  Engine or TableModel instance used for dispatch.

- .schema:

  Character. Name of the schema to check.

## Functions

- `check_schema_exists.mysql()`: Check if a schema exists for MySQL.

- `check_schema_exists.postgres()`: Check if a schema exists for
  PostgreSQL.

- `check_schema_exists.sqlite()`: SQLite does not support schemas;
  always returns TRUE
