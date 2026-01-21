# Define a foreign key column

Define a foreign key column

## Usage

``` r
ForeignKey(
  type,
  ref_table = NULL,
  ref_column = NULL,
  references = NULL,
  on_delete = NULL,
  on_update = NULL,
  ...
)
```

## Arguments

- type:

  SQL data type (e.g. "INTEGER")

- ref_table:

  Character. Name of the referenced table.

- ref_column:

  Character. Name of the referenced column. Used when specifying the
  pieces separately.

- references:

  "table.column" string specifying the referenced field. This is the
  recommended way to declare the target.

- on_delete:

  Optional ON DELETE behavior (e.g. "CASCADE")

- on_update:

  Optional ON UPDATE behavior

- ...:

  Reserved for extras like CHECK, COLLATE, etc.

## Value

A ForeignKey object

## Details

This function creates a ForeignKey object, which is a special type of
Column. It inherits all properties of a Column and adds foreign key
specific attributes. See
[`Column`](https://kent-orr.github.io/oRm/reference/Column.md) for
details on additional parameters that can be passed via `...`.

## See also

[`Column`](https://kent-orr.github.io/oRm/reference/Column.md)

## Examples

``` r
# Define a foreign key referencing the 'id' column in the 'users' table
user_id_fk <- ForeignKey("INTEGER", references = "users.id", on_delete = "CASCADE")

# Define a nullable foreign key with custom update behavior
category_id_fk <- ForeignKey(
  "INTEGER", references = "categories.id",
  nullable = TRUE, on_update = "SET NULL"
)
```
