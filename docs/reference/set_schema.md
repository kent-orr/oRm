# Set schema for database operations

This internal function sets the schema context for database operations,
using dialect-specific logic for different database systems.

## Usage

``` r
set_schema.mssql(x, .schema)

set_schema.mysql(x, .schema)

set_schema.postgres(x, .schema)

set_schema(x, .schema)

set_schema.default(x, .schema)

set_schema.sqlite(x, .schema)
```

## Arguments

- x:

  An oRm object (Engine, TableModel, or Record) used for dialect
  dispatch

- .schema:

  Character string of the schema name to set

## Value

Invisible NULL (called for side effects)

## Functions

- `set_schema.mssql()`: SQL Server has no session-level schema switch
  (the default schema is a property of the login), so schemas are
  applied purely by name qualification, which \`qualify.default\`
  already handles.

- `set_schema.mysql()`: Change the active schema using MySQL's USE
  statement.

- `set_schema.postgres()`: PostgreSQL applies schema via search_path.

- `set_schema.sqlite()`: SQLite does not support schemas
