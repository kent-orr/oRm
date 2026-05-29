# Reflect the columns of an existing table into Column objects

Inspects an existing database table and returns a named list of
\[Column\] objects, one per column. Used by \`Engine\$hydrate()\` to
build a TableModel from a table that already exists in the database.

## Usage

``` r
reflect_columns.postgres(x, tablename, ...)

reflect_columns(x, tablename, ...)

reflect_columns.default(x, tablename, ...)
```

## Arguments

- x:

  An Engine (or other oRm object) used for dialect dispatch.

- tablename:

  Character. Name of the table to reflect, already qualified.

- ...:

  Additional arguments for dialect-specific implementations.

## Value

A named list of \[Column\] objects, keyed by column name.

## Details

The default implementation is dialect-agnostic: it issues a zero-row
query and reads \`DBI::dbColumnInfo()\` to recover column names and
best-effort types. The reported type is backend-dependent (e.g. RSQLite
reports R types such as "double"); since hydration targets existing
tables, the type is not used to create the table and so is informational
only. Dialects may provide richer implementations (capturing primary
keys, nullability, and defaults).

## Functions

- `reflect_columns.postgres()`: PostgreSQL reflection via
  \`pg_catalog\`, capturing canonical types, primary keys, nullability,
  defaults, and foreign keys (returned as \[ForeignKey\] objects,
  schema-qualified when the target lives in another schema).
