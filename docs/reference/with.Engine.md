# Transaction Function

This function allows you to execute a block of code within a
transaction. If auto_commit is TRUE (default), the transaction will be
committed automatically upon successful execution. If auto_commit is
FALSE, you must explicitly commit or rollback within the transaction
block.

## Usage

``` r
# S3 method for class 'Engine'
with(data, expr, auto_commit = TRUE, ...)
```

## Arguments

- data:

  An Engine object that manages the database connection

- expr:

  An expression to be evaluated within the transaction

- auto_commit:

  Logical. Whether to automatically commit if no errors occur (default:
  TRUE)

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
