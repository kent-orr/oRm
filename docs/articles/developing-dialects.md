# Developing Dialects

`oRm` supports multiple SQL dialects through a lightweight dispatch
system. This vignette is aimed at developers who want to add support for
a new database backend.

## Dialect dispatch

Dialects are selected from the context of the object calling a helper.
The internal
[`dispatch_method()`](https://kent-orr.github.io/oRm/reference/dispatch_method.md)
looks for a function with the naming pattern `method.dialect` and falls
back to `method.default` when a dialect-specific method is not defined.

``` r
# simplified dispatch
dispatch_method <- function(x, method, ...) {
    dialect <- get_dialect(x)
    method_name <- paste(method, dialect, sep = '.')
    method_fn <- get0(method_name, mode = "function")
    if (is.null(method_fn)) {
        method_fn <- get0(paste0(method, '.default'), mode = "function")
    }
    method_fn(x, ...)
}
```

The dialect is typically stored on an `Engine` and propagates to
associated `TableModel` and `Record` objects.

## Required methods

To register a new dialect, create a file like `R/Dialect-mydialect.R`
and implement methods using the dispatch naming convention. The
following functions are commonly needed:

- `flush.mydialect(x, table, data, con, commit = TRUE, ...)`: Insert a
  row and return inserted data or identifiers.
- `qualify.mydialect(x, tablename, .schema)`: Qualify a table name with
  its schema if supported.
- `set_schema.mydialect(x, .schema)`: Switch the current schema on the
  connection.
- `check_schema_exists.mydialect(x, .schema)`: (Optional) Check whether
  a schema exists.

Each function will be called through
[`dispatch_method()`](https://kent-orr.github.io/oRm/reference/dispatch_method.md)
when working with engines, models, or records that declare your dialect.

## Documenting methods

Default implementations should include a roxygen block with `@rdname` so
they create the base manual page. Dialect-specific variants then add
their notes with `@describeIn`, which appends a subsection to the same
page and keeps everything discoverable via `?` or F1.

``` r
#' @rdname check_schema_exists
check_schema_exists.default <- function(x, .schema) {
    # ...
}

#' @describeIn check_schema_exists Check if a schema exists for PostgreSQL.
check_schema_exists.postgres <- function(x, .schema) {
    # ...
}
```

Running
[`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
after adding a dialect updates the shared documentation so new sections
appear automatically.

## Submitting a dialect

After implementing these functions, ensure the new dialect file is
listed in the `Collate:` field of `DESCRIPTION` and export any
user-facing helpers. Dialects are peer-reviewed through pull
requests—include tests demonstrating inserts, schema qualification, and
other behaviour specific to your backend.
