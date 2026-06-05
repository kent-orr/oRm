# Changelog

## oRm 0.6.0

### Breaking Changes

- **`Engine$hydrate()` renamed to `Engine$reflect()`** (and
  `Engine$hydrate_schema()` to `Engine$reflect_schema()`). The public
  verb now matches the internal reflection vocabulary it has always used
  ([`reflect_columns()`](https://kent-orr.github.io/oRm/reference/reflect_columns.md),
  [`reflect_tables()`](https://kent-orr.github.io/oRm/reference/reflect_tables.md))
  and the established ORM term for schema introspection (SQLAlchemy’s
  `MetaData.reflect()`). “Hydrate” conventionally means populating an
  object instance from a row, which is not what this method does — it
  builds a model from table metadata. There is no deprecation shim;
  update calls from `engine$hydrate(...)` to `engine$reflect(...)`.

### New Features

- **Set-level CRUD on `TableModel`** — the CRUD spine is now complete
  and discoverable at the model level, mirroring the row-level `Record`
  verbs:
  - `Model$create(...)` inserts a row (sugar over
    `Model$record(...)$create()`) and returns the persisted `Record`.
  - `Model$update(...)` issues a single `UPDATE ... WHERE`. Bare
    expressions are the WHERE filter (like `read()`); named arguments
    are the SET values (like `create()`),
    e.g. `User$update(id == 1, name = "Kent", age = 35)`.
  - `Model$delete(...)` issues a single `DELETE ... WHERE` from
    bare-expression filters.
  - [`update()`](https://rdrr.io/r/stats/update.html)/`delete()` return
    the affected-row count invisibly and refuse a filterless whole-table
    write unless `.all = TRUE`. Both require a primary key.
- **`TableModel$define_relationship()` method form** —
  [`define_relationship()`](https://kent-orr.github.io/oRm/reference/define_relationship.md)
  is now exposed as a model method that supplies the local model as
  `self`, making it discoverable via `$`-autocomplete per the
  verb-mirroring convention. The standalone
  [`define_relationship()`](https://kent-orr.github.io/oRm/reference/define_relationship.md)
  function is retained for functional/pipe-style use and internal
  wiring; both share one implementation.

### Bug Fixes

- **Double-qualification of table names in `Engine`** — `Engine$model()`
  and `Engine$reflect()` qualified the tablename before passing it to
  `TableModel$new()`, which qualified it again. Qualification now
  happens once, inside `TableModel$new()`; `reflect()` keeps a locally
  qualified name only for column reflection.

- **Single-column `read()`** — reads that project to a single column no
  longer collapse to an unnamed vector when building a `Record`. The
  `get`, `one_or_none`, and `all` modes now use `drop = FALSE` so the
  column name is preserved.

### Documentation

- Renamed the “Hydrating Models from Existing Tables” vignette to
  “Reflecting Models from Existing Tables” and updated the README and
  [`vignette("using-engine")`](https://kent-orr.github.io/oRm/articles/using-engine.md)
  for the `reflect()` / `reflect_schema()` naming.

## oRm 0.5.0

### New Features

- **PostgreSQL rich table reflection** — `engine$hydrate()` now captures
  full column metadata when using the PostgreSQL dialect:
  - Canonical types (e.g. `integer`, `text`, `timestamp with time zone`)
  - Primary key flags, nullability, and column defaults
  - Server-side defaults (sequences, `now()`, etc.) stored as
    [`dbplyr::sql()`](https://dbplyr.tidyverse.org/reference/sql.html)
    objects and applied by the database at insert time
  - Foreign keys reflected as `ForeignKey` objects, schema-qualified
    when the target lives in another schema
- **Schema-qualified foreign keys** —
  [`ForeignKey()`](https://kent-orr.github.io/oRm/reference/ForeignKey.md)
  now accepts cross-schema targets:
  - New `ref_schema` argument:
    `ForeignKey("INTEGER", ref_schema = "other", ref_table = "users", ref_column = "id")`
  - Three-part `references` shorthand:
    `ForeignKey("INTEGER", references = "other.users.id")`
  - [`render_constraint()`](https://kent-orr.github.io/oRm/reference/render_constraint.md)
    emits `REFERENCES "other"."users" ("id")` for cross-schema FKs
- **`Engine$hydrate_schema()`** — hydrate an entire schema in one call
  and auto-wire relationships:
  - Hydrates all (or a selected subset of) tables via `hydrate()`
  - Turns every reflected `ForeignKey` into a `many_to_one` /
    `one_to_many` relationship pair via
    [`define_relationship()`](https://kent-orr.github.io/oRm/reference/define_relationship.md)
  - Accepts `tables`, `exclude`, `.schema`, and `wire_relationships`
    arguments
  - Foreign keys pointing at tables outside the hydrated set emit a
    warning and are skipped
- **[`reflect_tables()`](https://kent-orr.github.io/oRm/reference/reflect_tables.md)
  generic** — new dialect-dispatched function that lists the tables
  available in a schema:
  - Default implementation wraps
    [`DBI::dbListTables()`](https://dbi.r-dbi.org/reference/dbListTables.html)
  - PostgreSQL implementation queries `pg_catalog.pg_tables` for the
    target schema

### Improvements

- `Record` now skips server-side defaults
  ([`dbplyr::sql()`](https://dbplyr.tidyverse.org/reference/sql.html)
  objects) at insert time, letting the database compute the value and
  relying on
  [`flush()`](https://kent-orr.github.io/oRm/reference/flush.md) to read
  it back

### Documentation

- Updated README and
  [`vignette("using-engine")`](https://kent-orr.github.io/oRm/articles/using-engine.md)
  to document PostgreSQL rich reflection, schema-qualified foreign keys,
  and `hydrate_schema()`

------------------------------------------------------------------------

## oRm 0.4.0

### New Features

- Added [`Method()`](https://kent-orr.github.io/oRm/reference/Method.md)
  function for attaching custom methods to models at both table and
  record levels
  - Table-level methods operate on the entire table for custom queries
    and bulk operations
  - Record-level methods operate on individual records for
    instance-specific behavior
  - Both method types have access to `self` for calling model/record
    methods
- Added automatic JSON serialization/deserialization for PostgreSQL
  - JSON and JSONB columns automatically serialize R objects when
    writing to database
  - Automatically deserialize JSON to R objects when reading from
    database
- Added `Engine(.read_only = TRUE)` for read-only database access
  - Application-level guards block all INSERT, UPDATE, DELETE, CREATE
    TABLE, and DROP TABLE operations
  - Dialect-level enforcement: SQLite uses the `SQLITE_RO` open flag;
    PostgreSQL sets `default_transaction_read_only=on` via libpq; MySQL
    executes `SET SESSION TRANSACTION READ ONLY` post-connect
- Added partial model support: a `TableModel` can declare a subset of an
  existing table’s columns
  - `read()` projects results to only the declared fields, preventing
    undeclared columns from surfacing in `Record` objects
  - Useful for scoped, safe access to production tables with sensitive
    or irrelevant columns
- `create_table(overwrite = TRUE)` now prompts for confirmation in
  interactive sessions
  - Pass `ask = FALSE` to bypass the prompt in scripts and automated
    workflows

### Documentation

- Added new vignette “Using Methods” demonstrating method usage patterns
- Updated existing vignettes (Get Started, Using Records, Using
  TableModels) to include method documentation
- Added `Method` to core building blocks in package documentation

### Improvements

- Dropped stevedore dependency for PostgreSQL test setup

------------------------------------------------------------------------

## oRm 0.3.1

Initial CRAN release.
