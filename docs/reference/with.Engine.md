# Transaction Function

This function allows you to execute a block of code within a
transaction. If auto_commit is TRUE (default), the transaction will be
committed automatically upon successful execution. If auto_commit is
FALSE, you must explicitly commit or rollback within the transaction
block.

## Usage

``` r
# S3 method for class 'Engine'
with(data, expr, auto_commit = TRUE, .autoflush = NULL, ...)
```

## Arguments

- data:

  An Engine object that manages the database connection

- expr:

  An expression to be evaluated within the transaction

- auto_commit:

  Logical. Whether to automatically commit if no errors occur (default:
  TRUE)

- .autoflush:

  Logical. Whether \`Record\$create()\` calls inside the block should
  flush by default, populating records with server-generated values.
  Defaults to \`NULL\`, which inherits the surrounding setting – the
  engine's \`.autoflush\` value at the outermost block, or whatever an
  enclosing \`with.Engine()\` established. The previous value is
  restored when the block exits.

- ...:

  Additional arguments (ignored)

## Value

The result of evaluating the expression

## Details

Within the transaction block, the following special functions are
available:

- `commit()`: Explicitly commits the current transaction. After calling
  this function, no further changes will be made to the database within
  the current transaction block.

- `rollback()`: Explicitly rolls back (cancels) the current transaction.
  This undoes all changes made within the transaction block up to this
  point.

If `auto_commit = TRUE` (the default), the transaction will be
automatically committed when the block completes without errors. If an
error occurs, the transaction is automatically rolled back.

If `auto_commit = FALSE`, you must explicitly call `commit()` within the
block to save your changes. If neither `commit()` nor `rollback()` is
called, the transaction will be rolled back by default and a warning
will be issued.

## Server-generated values inside a transaction

\`Record\$create()\` resolves its \`flush_record\` argument from the
transaction state when it is left \`NULL\`. Outside a transaction the
insert is flushed and the record comes back populated with
server-generated values (IDENTITY and SERIAL keys, column defaults,
timestamps). Inside a \`with.Engine()\` block the default is a plain
insert, and those values stay \`NULL\` on the record – cheap for bulk
loads, but a surprise when a later statement needs the key.

There are three ways to get them back, innermost setting winning:

- \`create(flush_record = TRUE)\` on the individual insert that a later
  statement depends on – typically a parent row whose key a child row
  references.

- \`.autoflush = TRUE\` on the \`with.Engine()\` call, which makes every
  \`create()\` in the block flush by default. Individual calls can still
  opt out with \`flush_record = FALSE\`.

- \`Engine\$new(.autoflush = TRUE)\`, which does the same for every
  transaction on that engine, so callers never have to remember the
  flag. \`engine\$set_autoflush()\` changes it later in the session.

Flushing never commits: the insert joins the open transaction and is
rolled back with it. It does cost a round trip per insert that returns
the row, so \`.autoflush = TRUE\` is a poor fit for bulk loads where the
keys are unused.

## Examples

``` r
# \donttest{
# With auto-commit (default)
with.Engine(engine, {
  User$record(name = "Alice")$create()
  User$record(name = "Bob")$create()
  # Transaction automatically committed if no errors
})
#> Error in with.Engine(engine, {    User$record(name = "Alice")$create()    User$record(name = "Bob")$create()}): could not find function "with.Engine"

# Parent/child insert: flush the parent so its generated key is available
with.Engine(engine, {
  order <- Order$record(customer = "Alice")$create(flush_record = TRUE)
  Item$record(order_id = order$data$id, sku = "A-1")$create()
})
#> Error in with.Engine(engine, {    order <- Order$record(customer = "Alice")$create(flush_record = TRUE)    Item$record(order_id = order$data$id, sku = "A-1")$create()}): could not find function "with.Engine"

# Same thing for every insert in the block
with.Engine(engine, {
  order <- Order$record(customer = "Alice")$create()
  Item$record(order_id = order$data$id, sku = "A-1")$create()
}, .autoflush = TRUE)
#> Error in with.Engine(engine, {    order <- Order$record(customer = "Alice")$create()    Item$record(order_id = order$data$id, sku = "A-1")$create()}, .autoflush = TRUE): could not find function "with.Engine"

# With manual commit
with.Engine(engine, {
  User$record(name = "Alice")$create()
  User$record(name = "Bob")$create()
  
  # Explicitly commit the transaction
  commit()
}, auto_commit = FALSE)
#> Error in with.Engine(engine, {    User$record(name = "Alice")$create()    User$record(name = "Bob")$create()    commit()}, auto_commit = FALSE): could not find function "with.Engine"

# With conditional commit/rollback
with.Engine(engine, {
  User$record(name = "Alice")$create()
  
  # Check a condition
  if (some_validation_check()) {
    User$record(name = "Bob")$create()
    commit()
  } else {
    # Discard all changes if validation fails
    rollback()
  }
}, auto_commit = FALSE)
#> Error in with.Engine(engine, {    User$record(name = "Alice")$create()    if (some_validation_check()) {        User$record(name = "Bob")$create()        commit()    }    else {        rollback()    }}, auto_commit = FALSE): could not find function "with.Engine"

# Error handling with explicit rollback
with.Engine(engine, {
  tryCatch({
    User$record(name = "Alice")$create()
    # Some operation that might fail
    problematic_operation()
    commit()
  }, error = function(e) {
    # Custom error handling
    message("Operation failed: ", e$message)
    rollback()
  })
}, auto_commit = FALSE)
#> Error in with.Engine(engine, {    tryCatch({        User$record(name = "Alice")$create()        problematic_operation()        commit()    }, error = function(e) {        message("Operation failed: ", e$message)        rollback()    })}, auto_commit = FALSE): could not find function "with.Engine"
# }
```
