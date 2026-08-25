# Augment an 'ellmer' Chat with database CRUD tools

Optional integration with the ellmer package. Given an ellmer `Chat`
object and one or more oRm models (or an `Engine`),
`register_db_tools()` registers a small set of generic CRUD tools onto
the chat so the LLM can read, create, update, and delete rows. The chat
is modified in place (ellmer Chat objects are reference objects) and
returned invisibly.

## Usage

``` r
register_db_tools(chat, ..., tables = NULL, exclude = NULL, max_rows = 100)
```

## Arguments

- chat:

  An ellmer `Chat` object (e.g. from
  [`ellmer::chat_anthropic()`](https://ellmer.tidyverse.org/reference/chat_anthropic.html)).

- ...:

  One or more `TableModel` objects to expose, and/or a single `Engine`
  whose tables are reflected automatically via
  [`Engine`](https://kent-orr.github.io/oRm/reference/Engine.md)'s
  `reflect_schema()`.

- tables:

  Optional character vector limiting which tables to reflect when an
  `Engine` is supplied. Ignored for explicit `TableModel`s.

- exclude:

  Optional character vector of table names to skip when an `Engine` is
  supplied.

- max_rows:

  Integer. Maximum number of rows `db_read` returns to the LLM. Defaults
  to 100.

## Value

The `chat` object, invisibly.

## Details

The registered tools are table-parameterised rather than model-specific:

- `db_read(table, filter)`:

  Read rows. `filter` is an optional dplyr-style filter expression
  supplied as a string, e.g. `"age > 30 & name == 'Kent'"`. Results are
  returned as JSON.

- `db_create(table, values)`:

  Insert a row. `values` is a JSON object of column/value pairs, e.g.
  `'{"name":"Kent","age":35}'`.

- `db_update(table, filter, values)`:

  Update matching rows. A `filter` is required.

- `db_delete(table, filter)`:

  Delete matching rows. A `filter` is required.

The schema (tables and their columns) is embedded in each tool's
description so the LLM can discover what is available without an extra
round-trip.

Filter expressions are translated to SQL by dbplyr via oRm's existing
read/update/delete machinery, so the LLM can lean on its familiarity
with dplyr. Filters are parsed and evaluated in a minimal environment (a
child of [`baseenv()`](https://rdrr.io/r/base/environment.html)) so they
cannot reach objects in the caller's workspace.

Write permissions are governed entirely by the `Engine`: open it with
`.read_only = TRUE` to make `db_create`/`db_update`/ `db_delete` fail
with the engine's read-only error (which is returned to the LLM). There
is no separate permission layer here. Because the agent acts with the
caller's database privileges, prefer a read-only engine when exposing a
chat to untrusted input.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ellmer)
engine <- Engine$new(RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
User <- engine$model(
    "users",
    id = Column("INTEGER", primary_key = TRUE),
    name = Column("TEXT"),
    age = Column("INTEGER")
)
User$create_table()

chat <- chat_anthropic()
register_db_tools(chat, User)
chat$chat("Add a user named Kent who is 35, then list everyone over 30.")
} # }
```
