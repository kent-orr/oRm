#' Define a database column for a model
#'
#' Creates a `Column` object describing the type and constraints for a field
#' in a table model. Intended for use within `TableModel` or `BaseModel` definitions.
#'
#' @param type Character string representing the column's SQL data type
#'   (e.g. `"INTEGER"`, `"TEXT"`, `"VARCHAR"`).
#' @param default The default value to use if none is supplied when creating a record.
#'   If `NULL`, no default will be applied at the SQL level.
#' @param primary_key Logical. Whether this field is part of the primary key.
#'   Defaults to `FALSE`.
#' @param nullable Logical. Whether the column may accept `NULL` values.
#'   Defaults to `TRUE`.
#' @param unique Logical. Whether the column should be constrained as `UNIQUE`.
#'   Defaults to `FALSE`. Currently unused in table creation.
#' @param foreign_key Optional character string specifying a foreign key reference
#'   in the format `"referenced_table.referenced_column"`. Currently used for SQL generation.
#' @param on_delete Character string indicating `ON DELETE` behavior (e.g., `"CASCADE"`, `"SET NULL"`).
#'   Only relevant if `foreign_key` is set. Optional.
#' @param on_update Character string indicating `ON UPDATE` behavior (e.g., `"CASCADE"`, `"RESTRICT"`).
#'   Only relevant if `foreign_key` is set. Optional.
#' @param ... Reserved for future extensions, such as `check`, `collate`, or custom constraints.
#'
#' @return An object of class `"Column"`, used to define fields in a TableModel.
#'
#' @examples
#' Column("TEXT", nullable = FALSE)
#' Column("INTEGER", primary_key = TRUE)
#' Column("INTEGER", foreign_key = "users.id", on_delete = "CASCADE")
#'
#' @export
Column <- function(
  type, 
  default = NULL, 
  primary_key = FALSE, 
  nullable = TRUE, 
  unique = FALSE, 
  foreign_key = NULL, 
  on_delete = NULL, 
  on_update = NULL, 
  ...) {

  structure(
    # Internal structure with extended FK support
    list(
      type = type,
      nullable = nullable,
      primary_key = primary_key,
      default = default,
      nullable = nullable,
      unique = unique,
      foreign_key = foreign_key,
      on_delete = on_delete,
      on_update = on_update,
      extras = list(...)
    ),
    class = "Column"
  )
}