# oRm 0.5.0

## New Features

* **PostgreSQL rich table reflection** — `engine$hydrate()` now captures full column metadata when using the PostgreSQL dialect:
    - Canonical types (e.g. `integer`, `text`, `timestamp with time zone`)
    - Primary key flags, nullability, and column defaults
    - Server-side defaults (sequences, `now()`, etc.) stored as `dbplyr::sql()` objects and applied by the database at insert time
    - Foreign keys reflected as `ForeignKey` objects, schema-qualified when the target lives in another schema

* **Schema-qualified foreign keys** — `ForeignKey()` now accepts cross-schema targets:
    - New `ref_schema` argument: `ForeignKey("INTEGER", ref_schema = "other", ref_table = "users", ref_column = "id")`
    - Three-part `references` shorthand: `ForeignKey("INTEGER", references = "other.users.id")`
    - `render_constraint()` emits `REFERENCES "other"."users" ("id")` for cross-schema FKs

* **`Engine$hydrate_schema()`** — hydrate an entire schema in one call and auto-wire relationships:
    - Hydrates all (or a selected subset of) tables via `hydrate()`
    - Turns every reflected `ForeignKey` into a `many_to_one` / `one_to_many` relationship pair via `define_relationship()`
    - Accepts `tables`, `exclude`, `.schema`, and `wire_relationships` arguments
    - Foreign keys pointing at tables outside the hydrated set emit a warning and are skipped

* **`reflect_tables()` generic** — new dialect-dispatched function that lists the tables available in a schema:
    - Default implementation wraps `DBI::dbListTables()`
    - PostgreSQL implementation queries `pg_catalog.pg_tables` for the target schema

## Improvements

* `Record` now skips server-side defaults (`dbplyr::sql()` objects) at insert time, letting the database compute the value and relying on `flush()` to read it back

## Documentation

* Updated README and `vignette("using-engine")` to document PostgreSQL rich reflection, schema-qualified foreign keys, and `hydrate_schema()`

---

# oRm 0.4.0

## New Features

* Added `Method()` function for attaching custom methods to models at both table and record levels
    - Table-level methods operate on the entire table for custom queries and bulk operations
    - Record-level methods operate on individual records for instance-specific behavior
    - Both method types have access to `self` for calling model/record methods

* Added automatic JSON serialization/deserialization for PostgreSQL
    - JSON and JSONB columns automatically serialize R objects when writing to database
    - Automatically deserialize JSON to R objects when reading from database

* Added `Engine(.read_only = TRUE)` for read-only database access
    - Application-level guards block all INSERT, UPDATE, DELETE, CREATE TABLE, and DROP TABLE operations
    - Dialect-level enforcement: SQLite uses the `SQLITE_RO` open flag; PostgreSQL sets `default_transaction_read_only=on` via libpq; MySQL executes `SET SESSION TRANSACTION READ ONLY` post-connect

* Added partial model support: a `TableModel` can declare a subset of an existing table's columns
    - `read()` projects results to only the declared fields, preventing undeclared columns from surfacing in `Record` objects
    - Useful for scoped, safe access to production tables with sensitive or irrelevant columns

* `create_table(overwrite = TRUE)` now prompts for confirmation in interactive sessions
    - Pass `ask = FALSE` to bypass the prompt in scripts and automated workflows

## Documentation

* Added new vignette "Using Methods" demonstrating method usage patterns
* Updated existing vignettes (Get Started, Using Records, Using TableModels) to include method documentation
* Added `Method` to core building blocks in package documentation

## Improvements

* Dropped stevedore dependency for PostgreSQL test setup

---

# oRm 0.3.1

Initial CRAN release.
