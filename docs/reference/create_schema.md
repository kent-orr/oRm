# Create a schema for the current dialect

By default, this is a no-op. Implement dialect-specific ones as needed.

## Usage

``` r
create_schema.postgres(x, .schema)

create_schema(x, .schema)

create_schema.default(x, .schema)

create_schema.sqlite(x, .schema)
```

## Arguments

- x:

  An Engine or TableModel instance used for dispatch.

- .schema:

  Character. Name of the schema to create.

## Functions

- `create_schema.postgres()`: Create the schema for PostgreSQL.
  Suppresses notices when the schema already exists.

- `create_schema.sqlite()`: SQLite does not support schemas
