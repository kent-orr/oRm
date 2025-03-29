#' Record Class
#'
#' @description
#' The Record class represents a single row in a database table. It provides methods
#' for creating, updating, and deleting individual records.
#'
#' @details
#' Record is an R6 class that works in conjunction with the TableModel class. Each Record
#' instance corresponds to a single row in the database table represented by its associated
#' TableModel. The class provides methods for CRUD (Create, Read, Update, Delete) operations
#' on individual records.
#'
#' @field model A TableModel object. Represents the database table this record belongs to.
#' @field data A list. Contains the data for this record, with column names as keys.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(model, data = list())}}{Constructor for creating a new Record instance.}
#'   \item{\code{create()}}{Inserts this record into the database.}
#'   \item{\code{update()}}{Updates this record in the database.}
#'   \item{\code{delete()}}{Deletes this record from the database.}
#' }
#'
#' @importFrom DBI dbQuoteLiteral dbQuoteIdentifier dbExecute dbAppendTable
#' 
#' @export
Record <- R6::R6Class(
  "Record",
  public = list(
    relationships = c(),
    model = NULL,
    data = list(),

    #' @description Constructor for Record.
    #' @param model A TableModel object.
    #' @param .data A named list of field values.
    initialize = function(model, ..., .data = list()) {
      if (!inherits(model, "TableModel")) {
        stop("Record must be initialized with a TableModel.")
      }

      args = c(list(...), .data)

      unknown_cols <- setdiff(names(args), names(model$fields))
      if (length(unknown_cols) > 0) {
        stop("Unknown fields in data: ", paste(unknown_cols, collapse = ", "))
      }

      self$model <- model
      self$relationships = model$relationships
      for (field in model$fields) {
        if (isTRUE(field['default']))
          self$data[names(field)] <- field['default']
      }
      self$data <- args
    },


    #' @description Insert this record into the database.
    #' @return Invisible NULL
    create = function() {
      con <- self$model$get_connection()

      required_fields <- names(self$model$fields)[
        vapply(self$model$fields, function(x) !x$nullable, logical(1))
      ]
      missing_fields <- setdiff(required_fields, names(self$data))
      if (length(missing_fields) > 0) {
        stop("Missing required fields: ", paste(missing_fields, collapse = ", "))
      }

      DBI::dbAppendTable(
        conn = con,
        name = self$model$tablename,
        value = as.data.frame(self$data, stringsAsFactors = FALSE)
      )
      self
    },

    #' @description Update this record in the database.
    #' @return Invisible NULL
    update = function() {
      con <- self$model$get_connection()

      key_fields <- names(self$model$fields)[
        vapply(self$model$fields, function(x) isTRUE(x$primary_key), logical(1))
      ]
      if (length(key_fields) == 0) {
        stop("No primary key fields defined in model.")
      }

      missing_keys <- setdiff(key_fields, names(self$data))
      if (length(missing_keys) > 0) {
        stop("Cannot update without all primary key fields: ",
             paste(missing_keys, collapse = ", "))
      }

      non_key_fields <- setdiff(names(self$data), key_fields)
      if (length(non_key_fields) == 0) {
        stop("No non-key fields to update.")
      }

      set_clause <- paste0(
        non_key_fields, " = ",
        sapply(self$data[non_key_fields], DBI::dbQuoteLiteral, conn = con),
        collapse = ", "
      )

      where_clause <- paste0(
        key_fields, " = ",
        sapply(self$data[key_fields], DBI::dbQuoteLiteral, conn = con),
        collapse = " AND "
      )

      sql <- sprintf(
        "UPDATE %s SET %s WHERE %s",
        DBI::dbQuoteIdentifier(con, self$model$tablename),
        set_clause,
        where_clause
      )

      DBI::dbExecute(con, sql)
      self
    },

    #' @description Delete this record from the database.
    #' @return Invisible NULL
    delete = function() {
      con <- self$model$get_connection()

      key_fields <- names(self$model$fields)[
        vapply(self$model$fields, function(x) isTRUE(x$primary_key), logical(1))
      ]
      if (length(key_fields) == 0) {
        stop("No primary key fields defined in model.")
      }

      missing_keys <- setdiff(key_fields, names(self$data))
      if (length(missing_keys) > 0) {
        stop("Cannot delete without all primary key fields: ",
             paste(missing_keys, collapse = ", "))
      }

      where_clause <- paste0(
        key_fields, " = ",
        sapply(self$data[key_fields], DBI::dbQuoteLiteral, conn = con),
        collapse = " AND "
      )

      sql <- sprintf(
        "DELETE FROM %s WHERE %s",
        DBI::dbQuoteIdentifier(con, self$model$tablename),
        where_clause
      )

      DBI::dbExecute(con, sql)
      NULL
    },

    relationship = function(rel_name, ...) {
      relationship = self$model$relationships[[rel_name]]
      if (is.null(relationship)) stop("No relationship defined for '", rel_name, "'.", call. = FALSE)
      
      key_val = self$data[[relationship$local_key]]
      if (is.null(key_val)) return(NULL)
      
      filter_expr <- rlang::expr(!!rlang::sym(relationship$related_key) == !!key_val)
      relationship$related_model$read(!!filter_expr, ...)
    },

    print = function() {
      cat("<Record>: '", self$model$tablename, "'\n", sep='')
      cat(paste(names(self$data), self$data, sep = ": ", collapse = "\n"), "\n")
      invisible(self)
    }
  )
)
