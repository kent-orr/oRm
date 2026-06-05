# List the tables available in a schema

Returns the names of tables that can be reflected, used by
\`Engine\$reflect_schema()\`. The default implementation falls back to
\`DBI::dbListTables()\` and ignores \`.schema\`; dialects with
first-class schema support (e.g. PostgreSQL) provide a schema-scoped
implementation.

## Usage

``` r
reflect_tables.postgres(x, .schema = NULL, ...)

reflect_tables(x, .schema = NULL, ...)

reflect_tables.default(x, .schema = NULL, ...)
```

## Arguments

- x:

  An Engine (or other oRm object) used for dialect dispatch.

- .schema:

  Character. Schema whose tables to list, or NULL.

- ...:

  Additional arguments for dialect-specific implementations.

## Value

A character vector of (bare) table names.

## Functions

- `reflect_tables.postgres()`: List base tables in a PostgreSQL schema
  (defaults to \`current_schema()\`).
