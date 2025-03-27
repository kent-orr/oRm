#' Define a basic column for a database table
#'
#' @param type SQL data type (e.g. "INTEGER", "TEXT", "DATE")
#' @param default Optional default value (no SQL default if NULL)
#' @param primary_key Logical, whether this is the primary key
#' @param nullable Logical, whether NULLs are allowed (default TRUE)
#' @param unique Logical, whether the column is UNIQUE
#' @param ... Reserved for extras like CHECK, COLLATE, etc.
#'
#' @return A Column object
#' @export
Column <- function(type, default = NULL, primary_key = FALSE,
                   nullable = TRUE, unique = FALSE, ...) {
  structure(
    list(
      type = type,
      default = default,
      primary_key = primary_key,
      nullable = nullable,
      unique = unique,
      extras = list(...)
    ),
    class = "Column"
  )
}

#' Define a foreign key column
#'
#' @param type SQL data type (e.g. "INTEGER")
#' @param references Character. The referenced table and column (e.g. "users.id")
#' @param on_delete Optional ON DELETE behavior (e.g. "CASCADE")
#' @param on_update Optional ON UPDATE behavior
#' @param ... Passed to base Column (including nullable, primary_key, etc.)
#'
#' @return A ForeignKey object
#' @export
ForeignKey <- function(type, references,
                       on_delete = NULL, on_update = NULL, ...) {
  col <- Column(type, ...)
  class(col) <- c("ForeignKey", class(col))  # prepend class
  col$references <- references
  col$on_delete <- on_delete
  col$on_update <- on_update
  col
}