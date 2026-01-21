# Add conditional SQL parts to column definition

This internal helper function conditionally adds SQL fragments to a
column definition based on field properties.

## Usage

``` r
add_part(parts, field, true, false = NULL)
```

## Arguments

- parts:

  Character vector of existing SQL parts

- field:

  Logical value or NULL indicating whether to add the SQL fragment

- true:

  Character string to add when field is TRUE

- false:

  Character string to add when field is FALSE (optional)

## Value

Character vector with potentially added SQL parts
