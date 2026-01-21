# Get the dialect from an oRm object

This internal function extracts the database dialect from various oRm
objects by traversing their object hierarchy to find the associated
Engine.

## Usage

``` r
get_dialect(x)
```

## Arguments

- x:

  An oRm object (Engine, TableModel, or Record)

## Value

Character string representing the dialect name, or "default" if no
dialect is found
