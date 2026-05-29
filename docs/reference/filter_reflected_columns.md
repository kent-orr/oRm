# Filter reflected columns by include/exclude

Applies optional \`include\` then \`exclude\` filters to a named list of
reflected \[Column\] objects. Table order is preserved. Requested
\`include\` columns that are not present emit a warning and are ignored.

## Usage

``` r
filter_reflected_columns(cols, include = NULL, exclude = NULL)
```

## Arguments

- cols:

  Named list of Column objects.

- include:

  Optional character vector of column names to keep.

- exclude:

  Optional character vector of column names to drop.

## Value

The filtered named list of Column objects.
