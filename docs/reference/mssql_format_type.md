# Render a SQL Server type from its \`sys.columns\` metadata

Reconstructs the declared type, including length/precision/scale and any
IDENTITY specification, so a reflected model can recreate the column.

## Usage

``` r
mssql_format_type(
  type_name,
  max_length,
  precision,
  scale,
  is_identity = FALSE,
  seed = NULL,
  increment = NULL
)
```

## Arguments

- type_name:

  Base type name from \`sys.types\`.

- max_length:

  Byte length from \`sys.columns\`; -1 means MAX.

- precision, scale:

  Numeric precision and scale from \`sys.columns\`.

- is_identity:

  Logical, whether the column is an IDENTITY column.

- seed, increment:

  IDENTITY seed and increment from \`sys.identity_columns\`.

## Value

A character type declaration.
