# Define a basic column for a database table

Define a basic column for a database table

## Usage

``` r
Column(
  type,
  ...,
  default = NULL,
  primary_key = NULL,
  nullable = NULL,
  unique = NULL
)
```

## Arguments

- type:

  SQL data type (e.g. "INTEGER", "TEXT", "DATE")

- ...:

  Reserved for extras like CHECK, COLLATE, etc.

- default:

  Optional default value. No SQL default if NULL. Use a string for a
  literal default, a function for a computed default at record
  generation, or an unquoted SQL expression via \`dbplyr::sql()\`.

- primary_key:

  Logical, whether this is part of the primary key. NULL (default) means
  unspecified.

- nullable:

  Logical, whether NULLs are allowed. NULL (default) means unspecified.

- unique:

  Logical, whether the column has a UNIQUE constraint. NULL (default)
  means unspecified.

## Value

A Column object

## Details

When \`primary_key\`, \`nullable\`, or \`unique\` are set to NULL, the
behavior is left to the database system's defaults or determined by
higher-level functions. This allows for flexible column definitions and
supports composite primary keys.

## Examples

``` r
# Define a simple integer column
id_col <- Column("INTEGER", primary_key = TRUE, nullable = FALSE)

# Define a text column with a default value
name_col <- Column("TEXT", default = "Unnamed", nullable = FALSE)

# Define a timestamp column with an SQL function default
created_col <- Column("TIMESTAMP", default = dbplyr::sql("now()"))

# Define a unique email column
email_col <- Column("TEXT", unique = TRUE, nullable = FALSE)
```
