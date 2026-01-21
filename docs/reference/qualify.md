# Qualify table name with schema

This internal function adds schema qualification to table names when
needed, using dialect-specific logic for different database systems.

## Usage

``` r
qualify.mysql(x, tablename, .schema)

qualify.postgres(x, tablename, .schema)

qualify(x, tablename, .schema)

qualify.default(x, tablename, .schema)

qualify.sqlite(x, tablename, .schema)
```

## Arguments

- x:

  An oRm object (Engine, TableModel, or Record) used for dialect
  dispatch

- tablename:

  Character string of the table name to qualify

- .schema:

  Character string of the schema name, or NULL

## Value

Character string of the qualified table name

## Functions

- `qualify.mysql()`: MySQL-specific table qualification with optional
  schema.

- `qualify.postgres()`: Add the schema prefix to unqualified table names
  for PostgreSQL.

- `qualify.sqlite()`: SQLite ignores schema qualification
