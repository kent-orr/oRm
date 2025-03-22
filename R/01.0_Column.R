#' Define a database column for a model
#'
#' Creates a `Column` object describing the type and constraints for a field
#' in a table model. Intended for use within `BaseModel` definitions.
#'
#' @param type Character string representing the column's SQL data type (e.g. `"INTEGER"`, `"TEXT"`).
#' @param default The default value to use if none is supplied when creating a record. If `nullable = FALSE` and `default` is not provided, a basic default will be inferred from `type`.
#' @param key Logical, whether this field is part of the primary key. **Note:** currently not used when creating tables — for future support.
#' @param nullable Logical, whether the field is allowed to be `NULL`. Defaults to `TRUE`.
#' @param ... Reserved for future dialect-specific extensions (e.g. `unique`, `collate`, `check`). Currently stored but **not used** during table creation.
#'
#' @return An object of class `"Column"`, used to define fields in a `BaseModel`.
#'
#' @examples
#' Column("TEXT", nullable = FALSE)
#' Column("INTEGER", key = TRUE, autoincrement = TRUE)
#'
#' @export
Column <- function(type, default = NULL, key = FALSE, nullable = TRUE, ...) {
  extras <- list(...)

  if (is.null(default) && !nullable) {
    default <- switch(
      tolower(type),
      "integer"   = 0L,
      "real"      = 0.0,
      "numeric"   = 0.0,
      "text"      = "",
      "varchar"   = "",
      "date"      = "1970-01-01",
      "timestamp" = "1970-01-01 00:00:00",
      NULL
    )
  }

  structure(
    list(
      type = type,
      nullable = nullable,
      key = key,
      default = default,
      extras = extras
    ),
    class = "Column"
  )
}
