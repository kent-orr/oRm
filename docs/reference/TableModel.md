# TableModel Class

The TableModel class represents a database table in the oRm framework.
It manages table structure, fields, relationships, and provides methods
for interacting with the database table.

## Details

TableModel is a core component of the oRm framework, responsible for:

- Defining table structure with columns and relationships

- Creating and managing database tables

- Providing an interface for CRUD operations on table records

- Managing relationships between different tables

Key features:

- Dynamic table creation and management

- Support for various column types and constraints

- Relationship definitions and querying

- Record creation and retrieval

## Note

Errors if the table's schema does not exist.

Throws an error if zero or multiple records are found.

Throws an error if multiple records are found.

## Methods

- `initialize(tablename, engine, ..., .data = list(), .schema = NULL, .default_mode = "all")`:

  Constructor for creating a new TableModel instance.

- `get_connection()`:

  Retrieve the active database connection from the engine.

- `generate_sql_fields()`:

  Generate SQL field definitions for table creation.

- `create_table(if_not_exists = TRUE, overwrite = FALSE, verbose = FALSE)`:

  Create the associated table in the database.

- `record(..., .data = list())`:

  Create a new Record object associated with this model.

- `read(..., .mode = NULL, .limit = NULL)`:

  Read records from the table using dynamic filters. If \`.mode\` is
  NULL, uses \`default_mode\`.

- `relationship(rel_name, ...)`:

  Query related records based on defined relationships.

- [`print()`](https://rdrr.io/r/base/print.html):

  Print a concise summary of the model, including its fields.

## See also

[`Engine`](https://kent-orr.github.io/oRm/reference/Engine.md),
[`Record`](https://kent-orr.github.io/oRm/reference/Record.md),
[`Column`](https://kent-orr.github.io/oRm/reference/Column.md),
[`ForeignKey`](https://kent-orr.github.io/oRm/reference/ForeignKey.md)

Engine::get_connection

\[Record\$relationship()\]

\[Engine\$print()\], \[Record\$print()\].

## Public fields

- `tablename`:

  Fully qualified name of the table in the database.

- `schema`:

  Schema that namespaces the table; defaults to the engine's schema.

- `engine`:

  Engine instance providing connections and SQL dialect.

- `fields`:

  Named list of Column objects defining the table structure.

- `relationships`:

  Named list of Relationship objects linking to other models.

- `default_mode`:

  Default mode for reading records when \`.mode\` is NULL.

## Methods

### Public methods

- [`TableModel$new()`](#method-TableModel-new)

- [`TableModel$get_connection()`](#method-TableModel-get_connection)

- [`TableModel$set_schema()`](#method-TableModel-set_schema)

- [`TableModel$create_table()`](#method-TableModel-create_table)

- [`TableModel$drop_table()`](#method-TableModel-drop_table)

- [`TableModel$record()`](#method-TableModel-record)

- [`TableModel$tbl()`](#method-TableModel-tbl)

- [`TableModel$read()`](#method-TableModel-read)

- [`TableModel$get()`](#method-TableModel-get)

- [`TableModel$all()`](#method-TableModel-all)

- [`TableModel$one_or_none()`](#method-TableModel-one_or_none)

- [`TableModel$relationship()`](#method-TableModel-relationship)

- [`TableModel$print()`](#method-TableModel-print)

- [`TableModel$clone()`](#method-TableModel-clone)

------------------------------------------------------------------------

### Method `new()`

Constructor for a new TableModel.

#### Usage

    TableModel$new(
      tablename,
      engine,
      ...,
      .data = list(),
      .schema = NULL,
      .default_mode = c("all", "one_or_none", "get", "data.frame", "tbl")
    )

#### Arguments

- `tablename`:

  The name of the database table.

- `engine`:

  The Engine object for database connection.

- `...`:

  Column definitions.

- `.data`:

  a list of Column defintions

- `.schema`:

  Character. Schema to apply to the table name. Defaults to the engine's
  schema.

- `.default_mode`:

  Character. Default mode used when \`read()\` is called with \`.mode\`
  = NULL. Must be one of "all", "one_or_none", "get", "data.frame", or
  "tbl".

------------------------------------------------------------------------

### Method `get_connection()`

Retrieve the active database connection from the engine. Delegates to
the associated engine and respects schema and pooling settings.

#### Usage

    TableModel$get_connection(...)

#### Arguments

- `...`:

  Additional arguments passed to the engine's \`get_connection\` method.

------------------------------------------------------------------------

### Method [`set_schema()`](https://kent-orr.github.io/oRm/reference/set_schema.md)

Update the schema for this model and re-qualify the table name.

#### Usage

    TableModel$set_schema(.schema)

#### Arguments

- `.schema`:

  Character. New schema name to apply.

#### Returns

The TableModel object.

------------------------------------------------------------------------

### Method `create_table()`

Create the associated table in the database.

#### Usage

    TableModel$create_table(
      if_not_exists = TRUE,
      overwrite = FALSE,
      verbose = FALSE
    )

#### Arguments

- `if_not_exists`:

  Logical. If TRUE, only create the table if it doesn't exist. Default
  is TRUE.

- `overwrite`:

  Logical. If TRUE, drop the table if it exists and recreate it. Default
  is FALSE.

- `verbose`:

  Logical. If TRUE, return the SQL statement instead of executing it.
  Default is FALSE.

#### Returns

The TableModel object invisibly.

------------------------------------------------------------------------

### Method `drop_table()`

Drop the associated table from the database. Prompts for confirmation by
default if running interactively.

#### Usage

    TableModel$drop_table(ask = interactive())

#### Arguments

- `ask`:

  Logical. If TRUE (default in interactive sessions), prompts the user
  for confirmation before dropping the table.

#### Returns

Invisibly returns the result of \`DBI::dbExecute()\` if the table is
dropped, or \`NULL\` if the operation is canceled or skipped.

#### Examples

    \donttest{
    # Drop the "users" table after confirmation
    User$drop_table()

    # Force drop without confirmation
    User$drop_table(ask = FALSE)
    }

------------------------------------------------------------------------

### Method `record()`

Create a new Record object with this model.

#### Usage

    TableModel$record(..., .data = list())

#### Arguments

- `...`:

  Named values to initialize the record's data.

- `.data`:

  a named list of field values.

------------------------------------------------------------------------

### Method `tbl()`

Generate a dbplyr tbl() object to be consumed by the model.

#### Usage

    TableModel$tbl()

------------------------------------------------------------------------

### Method `read()`

Read records using dynamic filters and return in the specified mode.

#### Usage

    TableModel$read(
      ...,
      .mode = NULL,
      .limit = 100,
      .offset = 0,
      .order_by = list()
    )

#### Arguments

- `...`:

  Unquoted expressions for filtering.

- `.mode`:

  Mode for reading records. One of "all", "one_or_none", "get",
  "data.frame", or "tbl". If NULL, uses \`default_mode\`. "data.frame"
  returns the raw result of \`dplyr::collect()\` rather than Record
  objects. "tbl" returns the uncollected dbplyr table.

- `.limit`:

  Integer. Maximum number of records to return. Defaults to 100. NULL
  means no limit. Positive values return the first N records, negative
  values return the last N records.

- `.offset`:

  Integer. Offset for pagination. Default is 0.

- `.order_by`:

  Unquoted expressions for ordering. Defaults to NULL (no order). Calls
  dplyr::arrange() so can take multiple args / desc()

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Shortcut for retrieving a single record. Expects exactly one matching
record.

#### Usage

    TableModel$get(...)

#### Arguments

- `...`:

  Unquoted expressions for filtering. For models with a single primary
  key, the first unnamed argument can be the primary key value.

#### Returns

A Record object if exactly one record is found.

#### Examples

    \donttest{
    # Get by primary key value (if single primary key exists)
    user <- User$get(123)

    # Get by named filter
    user <- User$get(email == "user@example.com")

    # Get with multiple filters
    user <- User$get(name == "John", active == 1)
    }

------------------------------------------------------------------------

### Method [`all()`](https://rdrr.io/r/base/all.html)

Retrieve all records matching the given filters.

#### Usage

    TableModel$all(...)

#### Arguments

- `...`:

  Unquoted expressions for filtering.

#### Returns

A list of Record objects, or NULL if no records are found.

#### Examples

    \donttest{
    # Get all users
    users <- User$all()

    # Get all active users
    active_users <- User$all(active == 1)

    # Get users with specific criteria
    admin_users <- User$all(role == "admin", active == 1)
    }

------------------------------------------------------------------------

### Method `one_or_none()`

Retrieve zero or one record matching the given filters.

#### Usage

    TableModel$one_or_none(...)

#### Arguments

- `...`:

  Unquoted expressions for filtering.

#### Returns

A Record object if exactly one record is found, NULL if no records are
found.

#### Examples

    \donttest{
    # Get user by email (expects 0 or 1 result)
    user <- User$one_or_none(email == "user@example.com")

    # Returns NULL if no match
    user <- User$one_or_none(id == 999)

    # Errors if multiple matches
    user <- User$one_or_none(status == "active")  # Error if multiple active users
    }
    Retrieve related records based on a defined relationship.

------------------------------------------------------------------------

### Method `relationship()`

#### Usage

    TableModel$relationship(rel_name, ...)

#### Arguments

- `rel_name`:

  The name of the relationship to query.

- `...`:

  Additional arguments passed to the related model's read method.

#### Details

This method returns related records based on the relationship type: -
For 'belongs_to', 'owns', 'one_to_one', and 'many_to_one' relationships,
it returns a single Record object or NULL. - For 'one_to_many' and
'many_to_many' relationships, it returns a list of Record objects.

For per-record filtering based on existing data, use
\[Record\$relationship()\], which applies additional constraints.

#### Returns

A single Record, a list of Records, or NULL, depending on the
relationship type.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print a concise summary of the model, including the table name and
column names.

#### Usage

    TableModel$print(...)

#### Arguments

- `...`:

  Unused, present for compatibility.

#### Returns

The TableModel object, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TableModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples
