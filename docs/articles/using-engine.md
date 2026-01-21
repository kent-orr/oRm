# Using Engine

An `Engine` is a connection to a database. Under the hood, engines call
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html) to
create and manage connections. This gives you flexibility to use any
database supported by the `DBI` package. Connecting to a database with
`Engine` is effectively the same as using
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)
directly.

So why wrap it in an R6 object?

- To manage opening and closing of connections
- To optionally support connection pooling
- To abstract some repetitive or database-specific boilerplate

### Creating an Engine

When creating an Engine, there are two additional arguments worth
knowing:

- `persist = TRUE`: Keeps the connection open between operations like
  `model()` or `execute()`. This is **required** when using an in-memory
  SQLite database, otherwise the data will be lost between calls.

- `use_pool = TRUE`: Enables connection pooling (via the `pool`
  package), which improves performance in environments like Shiny by
  reusing open connections across sessions.

``` r
library(oRm)

engine <- Engine$new(
  drv = RSQLite::SQLite(),
  dbname = ":memory:",
  persist = TRUE
)

engine
#> <Engine>
#>   dialect: sqlite, schema: NULL, connected: FALSE
```

### Common Methods

The full API is documented in the Engine reference page, but here are
the most commonly used methods:

- `model()` Creates a `TableModel` object from the Engine. [See the
  TableModel
  vignette](https://kent-orr.github.io/oRm/articles/using-tablemodels.md)

- `get_connection()` Returns the underlying DBI connection. You can use
  this directly for raw SQL or with
  [`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html) for
  custom queries.

- `execute(sql)` A lightweight wrapper around
  [`DBI::dbExecute()`](https://dbi.r-dbi.org/reference/dbExecute.html)
  using the Engine’s connection.

- `get_query(sql)` Executes a query and returns the results as a
  data.frame using
  [`DBI::dbGetQuery()`](https://dbi.r-dbi.org/reference/dbGetQuery.html).

``` r

# Run a raw SQL statement
engine$execute("CREATE TABLE things (id INTEGER PRIMARY KEY, name TEXT)")

# Retrieve a result
df <- engine$get_query("SELECT * FROM things")
```

## Using with.Engine

`with.Engine` is an S3 method for managing transaction state. It allows
you to run code within a database transaction, and if an error occurs
during execution, it will automatically roll back the transaction.

This provides a safeguard against partial writes or inconsistent states.
If you’re familiar with Python’s `sqlalchemy`, the usage will feel
familiar.

``` r
with(engine, {
  user = Users$record(name = "John Doe")
  user$create()
})
```

You can also choose to manually commit or roll back transactions. This
gives you full control over error handling:

``` r
with(engine, {
  user = Users$record(name = "Jane Doe")
  tryCatch({
    user$create()
  }, error = function(e) {
    print(paste("An error occurred:", e$message))
    rollback()
  })
  commit()
}, auto_commit = FALSE)
```

This approach helps ensure that your data remains in sync with the
database state. If any part of the transaction fails, with.Engine() will
automatically clean up and alert you with a meaningful error.

## Dialects

Engines also store the dialect of the database they connect to. This is
used to translate SQL statements into the correct syntax for the
specific database backend.

Different databases handle data types, operators, and functions in
slightly different ways. `oRm`’s dialect system isn’t exhaustive, but it
covers key differences that affect features like flushing or
value-returning inserts.

For example: - PostgreSQL supports `RETURNING *` to fetch inserted rows
immediately. - SQLite does not support `RETURNING`, so `oRm` falls back
to using `last_insert_rowid()`.

These dialect-specific behaviors are handled automatically, so your code
can stay consistent regardless of the database in use.
