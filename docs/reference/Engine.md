# Engine Class

The Engine class is a core component of the oRm framework, responsible
for managing database connections and providing methods for interacting
with the database. It supports both direct connections and connection
pooling, offering flexibility in how database resources are managed.

Key features:

- Manages database connections (single or pooled)

- Provides methods for executing SQL queries and commands

- Allows creation of TableModel objects for ORM operations

- Supports persistent connections for improved performance

## See also

\[TableModel::new()\]

\[Engine\$model()\], \[reflect_columns()\]

\[Engine\$hydrate()\], \[define_relationship()\], \[reflect_tables()\]

\[TableModel\$print()\], \[Record\$print()\].

## Public fields

- `conn_args`:

  A list of arguments for establishing a connection

- `conn`:

  Active database connection or pool

- `use_pool`:

  Whether to use connection pooling

- `persist`:

  Whether to keep connections open between operations

- `dialect`:

  Database dialect in use

- `schema`:

  Default schema applied to tables

## Methods

### Public methods

- [`Engine$new()`](#method-Engine-new)

- [`Engine$get_connection()`](#method-Engine-get_connection)

- [`Engine$close()`](#method-Engine-close)

- [`Engine$list_tables()`](#method-Engine-list_tables)

- [`Engine$get_query()`](#method-Engine-get_query)

- [`Engine$execute()`](#method-Engine-execute)

- [`Engine$set_schema()`](#method-Engine-set_schema)

- [`Engine$create_schema()`](#method-Engine-create_schema)

- [`Engine$check_schema_exists()`](#method-Engine-check_schema_exists)

- [`Engine$model()`](#method-Engine-model)

- [`Engine$hydrate()`](#method-Engine-hydrate)

- [`Engine$hydrate_schema()`](#method-Engine-hydrate_schema)

- [`Engine$set_transaction_state()`](#method-Engine-set_transaction_state)

- [`Engine$get_transaction_state()`](#method-Engine-get_transaction_state)

- [`Engine$qualify()`](#method-Engine-qualify)

- [`Engine$format_tablename()`](#method-Engine-format_tablename)

- [`Engine$print()`](#method-Engine-print)

- [`Engine$clone()`](#method-Engine-clone)

------------------------------------------------------------------------

### Method `new()`

Create an Engine object

#### Usage

    Engine$new(
      ...,
      conn_args = list(),
      .schema = NULL,
      .read_only = FALSE,
      use_pool = FALSE,
      persist = FALSE
    )

#### Arguments

- `...`:

  Additional arguments to be passed to DBI::dbConnect

- `conn_args`:

  A list of arguments to be passed to DBI::dbConnect

- `.schema`:

  Character. The default schema to apply to child TableModel objects

- `.read_only`:

  Logical. If TRUE, the engine refuses non-SELECT statements and applies
  dialect-specific connection-level read-only enforcement (SQLite:
  \`SQLITE_RO\` open flag; PostgreSQL: \`SET SESSION CHARACTERISTICS AS
  TRANSACTION READ ONLY\`; MySQL: \`SET SESSION TRANSACTION READ
  ONLY\`).

- `use_pool`:

  Logical. Whether or not to make use of the pool package for
  connections to this engine

- `persist`:

  Logical. Whether to keep the connection open after operations
  (default: FALSE) Get a connection to the database

  Reapplies the configured schema on every connection retrieval to
  ensure consistency after reconnects.

------------------------------------------------------------------------

### Method `get_connection()`

#### Usage

    Engine$get_connection()

#### Returns

A DBIConnection object or a pool object

------------------------------------------------------------------------

### Method [`close()`](https://rdrr.io/r/base/connections.html)

Close the database connection or pool

#### Usage

    Engine$close()

#### Returns

NULL

------------------------------------------------------------------------

### Method `list_tables()`

List tables in the database connection

#### Usage

    Engine$list_tables()

#### Returns

A character vector of table names

------------------------------------------------------------------------

### Method `get_query()`

Execute a SQL query and return the result as a data.frame

#### Usage

    Engine$get_query(sql)

#### Arguments

- `sql`:

  SQL query

#### Returns

A data.frame

------------------------------------------------------------------------

### Method `execute()`

Execute a SQL query and return the number of rows affected

#### Usage

    Engine$execute(sql)

#### Arguments

- `sql`:

  SQL query

#### Returns

The number of rows affected

------------------------------------------------------------------------

### Method [`set_schema()`](https://kent-orr.github.io/oRm/reference/set_schema.md)

Set the default schema for the engine and active connection

#### Usage

    Engine$set_schema(.schema)

#### Arguments

- `.schema`:

  Character. Schema name to apply

#### Returns

The Engine object

------------------------------------------------------------------------

### Method [`create_schema()`](https://kent-orr.github.io/oRm/reference/create_schema.md)

Explicitly create a schema in the database

#### Usage

    Engine$create_schema(.schema)

#### Arguments

- `.schema`:

  Character. The schema name to create

#### Returns

TRUE (invisible) if schema created/existed

------------------------------------------------------------------------

### Method [`check_schema_exists()`](https://kent-orr.github.io/oRm/reference/check_schema_exists.md)

Check if a schema exists in the database

#### Usage

    Engine$check_schema_exists(.schema)

#### Arguments

- `.schema`:

  Character. The schema name to check

#### Returns

TRUE if schema exists, otherwise FALSE

------------------------------------------------------------------------

### Method `model()`

Create a new TableModel object for the specified table

#### Usage

    Engine$model(
      tablename,
      ...,
      .data = list(),
      .schema = NULL,
      .default_mode = "all"
    )

#### Arguments

- `tablename`:

  Name of the table

- `...`:

  Additional arguments passed to the TableModel constructor. Include
  \`Column\` objects here to define the table structure.

- `.data`:

  A named list of the arguments for the TableModel constructor.
  \`Column\` objects in this list also define table structure.

- `.schema`:

  Character. The default schema to apply to the TableModel object

- `.default_mode`:

  Character. Default read mode for the TableModel.

#### Returns

A new TableModel object

#### Examples

    \donttest{
    engine$model(
        "users",
        Column$new("id", "integer"),
        Column$new("name", "text")
    )
    }

------------------------------------------------------------------------

### Method `hydrate()`

Build a TableModel by introspecting an existing database table.

Inspects the columns of an existing table and returns a ready-to-use
TableModel, so basic CRUD is possible without pre-defining columns. The
default reflection is best-effort and dialect-agnostic (column names and
reported types only; see \[reflect_columns()\]). The PostgreSQL dialect
reflects richer metadata: canonical types, primary keys, nullability,
defaults, and foreign keys (as \[ForeignKey\] objects).

#### Usage

    Engine$hydrate(
      tablename,
      ...,
      include = NULL,
      exclude = NULL,
      .schema = NULL,
      .default_mode = "all"
    )

#### Arguments

- `tablename`:

  Name of the existing table to hydrate.

- `...`:

  Additional \`Column\`, \`ForeignKey\`, or \`Method\` objects to merge
  in. These take precedence over reflected columns of the same name,
  matching the behaviour of \`model()\`.

- `include`:

  Optional character vector of column names to keep.

- `exclude`:

  Optional character vector of column names to drop.

- `.schema`:

  Character. The default schema to apply to the TableModel.

- `.default_mode`:

  Character. Default read mode for the TableModel.

#### Returns

A new TableModel object.

#### Examples

    \donttest{
    # Given a table "users" that already exists in the database:
    Users <- engine$hydrate("users")
    Users <- engine$hydrate("users", include = c("id", "name"))
    Users <- engine$hydrate("users", exclude = c("hash", "configuration"))
    }

------------------------------------------------------------------------

### Method `hydrate_schema()`

Hydrate several tables from a schema at once and wire up the
relationships implied by their foreign keys.

Each table is hydrated via \[Engine\$hydrate()\]; afterwards, every
reflected \[ForeignKey\] whose target is also among the hydrated models
is turned into a \`many_to_one\` relationship (with the reverse
\`one_to_many\` backref) using \[define_relationship()\]. Foreign keys
pointing at tables outside the hydrated set are skipped with a warning.
This is most useful with the PostgreSQL dialect, whose reflection
captures foreign keys.

#### Usage

    Engine$hydrate_schema(
      tables = NULL,
      ...,
      .schema = NULL,
      exclude = NULL,
      .default_mode = "all",
      wire_relationships = TRUE
    )

#### Arguments

- `tables`:

  Optional character vector limiting which tables to hydrate. When
  \`NULL\`, all tables in the schema are hydrated (see
  \[reflect_tables()\]).

- `...`:

  Additional \`Column\`, \`ForeignKey\`, or \`Method\` objects passed to
  every \`hydrate()\` call (overrides of reflected columns).

- `.schema`:

  Character. Schema to hydrate. Defaults to the engine schema.

- `exclude`:

  Optional character vector of table names to drop.

- `.default_mode`:

  Character. Default read mode for each TableModel.

- `wire_relationships`:

  Logical. Whether to auto-wire relationships from reflected foreign
  keys. Defaults to TRUE.

#### Returns

A named list of TableModel objects, keyed by bare table name.

#### Examples

    \donttest{
    models <- engine$hydrate_schema()
    models <- engine$hydrate_schema(tables = c("users", "posts"))
    posts <- models$posts
    }

------------------------------------------------------------------------

### Method `set_transaction_state()`

Set the internal transaction state

#### Usage

    Engine$set_transaction_state(state)

#### Arguments

- `state`:

  Logical. Indicates if a transaction is active

#### Returns

NULL

------------------------------------------------------------------------

### Method `get_transaction_state()`

Retrieve the current transaction state

#### Usage

    Engine$get_transaction_state()

#### Returns

Logical indicating if a transaction is active

------------------------------------------------------------------------

### Method [`qualify()`](https://kent-orr.github.io/oRm/reference/qualify.md)

Qualify a table name with a schema

#### Usage

    Engine$qualify(tablename, .schema = self$schema)

#### Arguments

- `tablename`:

  Character. Table name to qualify

- `.schema`:

  Character. Schema name to prepend

#### Returns

A fully qualified table name

------------------------------------------------------------------------

### Method `format_tablename()`

Quote and format a schema-qualified table name

#### Usage

    Engine$format_tablename(tablename)

#### Arguments

- `tablename`:

  Character. Table name to format

#### Returns

A quoted table name

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print a concise summary of the engine, including the SQL dialect,
default schema, and connection status.

#### Usage

    Engine$print(...)

#### Arguments

- `...`:

  Unused, present for compatibility.

#### Returns

The Engine object, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Engine$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## ------------------------------------------------
## Method `Engine$model`
## ------------------------------------------------

# \donttest{
engine$model(
    "users",
    Column$new("id", "integer"),
    Column$new("name", "text")
)
#> Error: object 'engine' not found
# }

## ------------------------------------------------
## Method `Engine$hydrate`
## ------------------------------------------------

# \donttest{
# Given a table "users" that already exists in the database:
Users <- engine$hydrate("users")
#> Error: object 'engine' not found
Users <- engine$hydrate("users", include = c("id", "name"))
#> Error: object 'engine' not found
Users <- engine$hydrate("users", exclude = c("hash", "configuration"))
#> Error: object 'engine' not found
# }

## ------------------------------------------------
## Method `Engine$hydrate_schema`
## ------------------------------------------------

# \donttest{
models <- engine$hydrate_schema()
#> Error: object 'engine' not found
models <- engine$hydrate_schema(tables = c("users", "posts"))
#> Error: object 'engine' not found
posts <- models$posts
#> Error: object 'models' not found
# }
```
