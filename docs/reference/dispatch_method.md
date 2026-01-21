# Dispatch method calls based on dialect

This internal function provides a mechanism for method dispatch based on
the dialect of the database engine. It looks for dialect-specific
implementations first, then falls back to default implementations.

## Usage

``` r
dispatch_method(x, method, ...)
```

## Arguments

- x:

  An object that has an associated dialect (Engine, TableModel, or
  Record)

- method:

  Character string naming the method to dispatch

- ...:

  Additional arguments passed to the dispatched method

## Value

The result of calling the appropriate dialect-specific or default method
