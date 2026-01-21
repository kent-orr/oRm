# Record Class

The Record class represents a single row in a database table. It
provides methods for creating, updating, and deleting individual
records.

## Details

Record is an R6 class that works in conjunction with the TableModel
class. Each Record instance corresponds to a single row in the database
table represented by its associated TableModel. The class provides
methods for CRUD (Create, Read, Update, Delete) operations on individual
records.

## Methods

- `initialize(model, data = list())`:

  Constructor for creating a new Record instance.

- `create()`:

  Inserts this record into the database.

- [`update()`](https://rdrr.io/r/stats/update.html):

  Updates this record in the database.

- `delete()`:

  Deletes this record from the database.

## See also

\[TableModel\$relationship()\]

\[Engine\$print()\], \[TableModel\$print()\].

## Public fields

- `model`:

  A TableModel object. Represents the database table this record belongs
  to.

- `data`:

  A list. Contains the data for this record, with column names as keys.

- `relationships`:

  A list. Contains the relationships defined for this record's model.

## Methods

### Public methods

- [`Record$new()`](#method-Record-new)

- [`Record$set_schema()`](#method-Record-set_schema)

- [`Record$create()`](#method-Record-create)

- [`Record$flush()`](#method-Record-flush)

- [`Record$update()`](#method-Record-update)

- [`Record$delete()`](#method-Record-delete)

- [`Record$refresh()`](#method-Record-refresh)

- [`Record$relationship()`](#method-Record-relationship)

- [`Record$print()`](#method-Record-print)

- [`Record$clone()`](#method-Record-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new Record instance.

#### Usage

    Record$new(model, ..., .data = list())

#### Arguments

- `model`:

  A TableModel object representing the database table.

- `...`:

  Named arguments corresponding to field values for the record.

- `.data`:

  A named list of field values (alternative to ...).

#### Returns

A new Record instance.

------------------------------------------------------------------------

### Method [`set_schema()`](https://kent-orr.github.io/oRm/reference/set_schema.md)

Update the schema for the underlying model.

#### Usage

    Record$set_schema(.schema)

#### Arguments

- `.schema`:

  Character. New schema name to apply.

------------------------------------------------------------------------

### Method `create()`

Insert this record into the database.

#### Usage

    Record$create(flush_record = NULL)

#### Arguments

- `flush_record`:

  Logical flag determining whether to call \`flush()\` after insertion.
  Defaults to \`NULL\`, which flushes when not currently in a
  transaction.

#### Returns

Invisible NULL

------------------------------------------------------------------------

### Method [`flush()`](https://kent-orr.github.io/oRm/reference/flush.md)

Flush this record's data to the database.

#### Usage

    Record$flush(commit = NULL)

#### Arguments

- `commit`:

  Logical. Whether to commit the transaction; defaults to NULL to
  automatically commit when not already in a transaction.

#### Returns

The Record instance (invisibly).

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Update this record in the database.

#### Usage

    Record$update(..., .data = list())

#### Arguments

- `...`:

  Named arguments corresponding to field values to update.

- `.data`:

  A named list of field values to update (alternative to ...).

#### Returns

The Record instance (invisibly).

------------------------------------------------------------------------

### Method `delete()`

Delete this record from the database.

#### Usage

    Record$delete()

#### Returns

Invisible NULL

------------------------------------------------------------------------

### Method `refresh()`

Refresh this record from the database.

#### Usage

    Record$refresh()

#### Details

Re-fetches this record from the database using its primary key,
replacing the current data and relationships. Any unsaved local changes
are discarded, and an error is raised if the record cannot be found.

#### Returns

The Record instance (invisibly).

------------------------------------------------------------------------

### Method `relationship()`

Retrieve related records based on a defined relationship.

#### Usage

    Record$relationship(rel_name, ...)

#### Arguments

- `rel_name`:

  The name of the relationship to query.

- `...`:

  Additional arguments passed to the related model's read method.

#### Details

This method returns related records based on the relationship type: -
For 'one_to_one' and 'many_to_one' relationships, it returns a single
Record object or NULL. - For 'one_to_many' and 'many_to_many'
relationships, it returns a list of Record objects.

#### Returns

A single Record, a list of Records, or NULL, depending on the
relationship type.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print a concise summary of the record, including the table name and
field values.

#### Usage

    Record$print(...)

#### Arguments

- `...`:

  Additional arguments passed to other print methods.

#### Returns

The Record object, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Record$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
