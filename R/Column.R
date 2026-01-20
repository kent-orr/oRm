#' Define a basic column for a database table
#'
#' @param type SQL data type (e.g. "INTEGER", "TEXT", "DATE")
#' @param default Optional default value. No SQL default if NULL. Use a string for a literal default, a function for a computed default at record generation, or an unquoted SQL expression via `dbplyr::sql()`.
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
#' # Define a timestamp column with an SQL function default
#' created_col <- Column("TIMESTAMP", default = dbplyr::sql("now()"))
#'
#' # Define a unique email column
#' email_col <- Column("TEXT", unique = TRUE, nullable = FALSE)
Column <- function(
    type, ..., default = NULL, primary_key = NULL,
    nullable = NULL, unique = NULL) {
  if (is.logical(primary_key)) {
    nullable <- NULL
  }
  structure(
    list(
      name = NULL,
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
validate_identifier <- function(x, arg = "identifier") {
  if (!is.character(x) || length(x) != 1 ||
      !grepl("^[A-Za-z_][A-Za-z0-9_]*$", x)) {
    stop(
      sprintf(
        "Invalid %s '%s'. Identifiers must start with a letter or underscore and contain only letters, numbers, or underscores.",
        arg, x
      ),
      call. = FALSE
    )
  }
  x
}



#' Define a foreign key column
#'
#' @inheritParams Column
#' @param type SQL data type (e.g. "INTEGER")
#' @param references "table.column" string specifying the referenced field.
#'   This is the recommended way to declare the target.
#' @param ref_table Character. Name of the referenced table.
#' @param ref_column Character. Name of the referenced column.
#'   Used when specifying the pieces separately.
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
#' category_id_fk <- ForeignKey(
#'   "INTEGER", references = "categories.id",
#'   nullable = TRUE, on_update = "SET NULL"
#' )
ForeignKey <- function(type, ref_table = NULL, ref_column = NULL, references = NULL,
                       on_delete = NULL, on_update = NULL, ...) {
  if (!is.null(references)) {
    if (!is.null(ref_table) || !is.null(ref_column)) {
      stop("Use either 'references' or 'ref_table'/'ref_column', not both.")
    }
    parts <- strsplit(references, "\\.")[[1]]
    if (length(parts) != 2) {
      stop("Invalid 'references' format. Expected 'table.column'.")
    }
    ref_table <- parts[1]
    ref_column <- parts[2]
  }

  if (is.null(ref_table) || is.null(ref_column)) {
    stop("ForeignKey requires 'ref_table' and 'ref_column'.")
  }

  ref_table <- validate_identifier(ref_table, "ref_table")
  ref_column <- validate_identifier(ref_column, "ref_column")

  col <- Column(type, ...)
  class(col) <- c("ForeignKey", class(col))  # prepend class
  col$ref_table <- ref_table
  col$ref_column <- ref_column
  col$references <- paste(ref_table, ref_column, sep = ".")
  col$on_delete <- on_delete
  col$on_update <- on_update
  col

}


#' Define a method to attach to a model
#'
#' @param fn A function containing the method implementation
#' @param target Character. Either "record" for instance methods or "table" for model-level methods
#'
#' @details
#' The `Method()` function provides explicit attachment of custom methods to ORM models.
#' Methods defined with `target = "record"` become instance methods available on individual
#' Record objects and have access to `self` (the record instance). Methods defined with
#' `target = "table"` become class methods available on the TableModel itself.
#'
#' Within method functions, `self` refers to either the Record (for "record" target) or
#' TableModel (for "table" target), following R6 conventions.
#'
#' @return An orm_method object
#' @export
#'
#' @examples
#' # Define a Students model with custom methods
#' \dontrun{
#' Students <- engine$model(
#'   "students",
#'   id = Column("INTEGER", primary_key = TRUE),
#'   name = Column("TEXT"),
#'   present = Column("INTEGER", default = 0),
#'
#'   # Instance method - available on individual student records
#'   greet = Method(function() {
#'     paste("Hello, I'm", self$name)
#'   }, target = "record"),
#'
#'   # Instance method - modify record state
#'   mark_present = Method(function() {
#'     self$present <- 1
#'     self$save()
#'   }, target = "record"),
#'
#'   # Class method - available on the Students model
#'   find_by_name = Method(function(name) {
#'     self$read(name = name)
#'   }, target = "table")
#' )
#'
#' # Using the methods:
#' student <- Students$read_one(id = 1)
#' student$greet()  # "Hello, I'm Alice"
#' student$mark_present()  # Updates and saves the record
#'
#' Students$find_by_name("Bob")  # Returns matching records
#' }
Method <- function(fn, target = c("record", "table")) {
  # Validate that fn is a function
  if (!is.function(fn)) {
    stop("The 'fn' parameter must be a function.", call. = FALSE)
  }

  # Validate target parameter
  target <- match.arg(target)

  structure(
    list(
      name = NULL,  # Will be set by TableModel$initialize
      fn = fn,
      target = target
    ),
    class = "orm_method"
  )
}
