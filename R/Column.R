#' Define a basic column for a database table
#'
#' @param type SQL data type (e.g. "INTEGER", "TEXT", "DATE")
#' @param default Optional default value. No SQL default if NULL, def set by string, if given a function that fun will be called by the Record on generation
#' @param primary_key Logical, whether this is part of the primary key. NULL (default) means unspecified.
#' @param nullable Logical, whether NULLs are allowed. NULL (default) means unspecified.
#' @param unique Logical, whether the column has a UNIQUE constraint. NULL (default) means unspecified.
#' @param ... Reserved for extras like CHECK, COLLATE, etc.
#'
#' @details
#' When `primary_key`, `nullable`, or `unique` are set to NULL, the behavior is left to the database system's defaults
#' or determined by higher-level functions. This allows for flexible column definitions and supports composite primary keys.
#'
#' @return A Column object
#' @export
#'
#' @examples
#' # Define a simple integer column
#' id_col <- Column("INTEGER", primary_key = TRUE, nullable = FALSE)
#'
#' # Define a text column with a default value
#' name_col <- Column("TEXT", default = "Unnamed", nullable = FALSE)
#'
#' # Define a unique email column
#' email_col <- Column("TEXT", unique = TRUE, nullable = FALSE)
Column <- function(type, ..., default = NULL, primary_key = NULL,
                   nullable = NULL, unique = NULL) {
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
#' @inheritParams Column
#' @param type SQL data type (e.g. "INTEGER")
#' @param references Character. The referenced table and column (e.g. "users.id")
#' @param on_delete Optional ON DELETE behavior (e.g. "CASCADE")
#' @param on_update Optional ON UPDATE behavior
#'
#' @details
#' This function creates a ForeignKey object, which is a special type of Column.
#' It inherits all properties of a Column and adds foreign key specific attributes.
#' See \code{\link{Column}} for details on additional parameters that can be passed via \code{...}.
#'
#' @return A ForeignKey object
#' @export
#'
#' @seealso \code{\link{Column}}
#'
#' @examples
#' # Define a foreign key referencing the 'id' column in the 'users' table
#' user_id_fk <- ForeignKey("INTEGER", references = "users.id", on_delete = "CASCADE")
#'
#' # Define a nullable foreign key with custom update behavior
#' category_id_fk <- ForeignKey("INTEGER", references = "categories.id",
#'                              nullable = TRUE, on_update = "SET NULL")

ForeignKey <- function(type, references,
                       on_delete = NULL, on_update = NULL, ...) {
  col <- Column(type, ...)
  class(col) <- c("ForeignKey", class(col))  # prepend class
  col$references <- references
  col$on_delete <- on_delete
  col$on_update <- on_update
  col

}
