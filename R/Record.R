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
#' @field relationships A list. Contains the relationships defined for this record's model.

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
    
    #' @description
    #' Initialize a new Record instance.
    #'
    #' @param model A TableModel object representing the database table.
    #' @param ... Named arguments corresponding to field values for the record.
    #' @param .data A named list of field values (alternative to ...).
    #'
    #' @return A new Record instance.
    initialize = function(model, ..., .data = list()) {
      if (!inherits(model, "TableModel")) {
        stop("Record must be initialized with a TableModel.")
      }
      
      args = utils::modifyList(.data, rlang::list2(...))
      
      unknown_cols <- setdiff(names(args), names(model$fields))
      if (length(unknown_cols) > 0) {
        stop("Unknown fields in data: ", paste(unknown_cols, collapse = ", "))
      }
      
      self$model <- model
      self$relationships = model$relationships
      field_names <- names(model$fields)
      for (i in seq_along(model$fields)) {
        field <- model$fields[[i]]
        field_name <- field_names[i]
        if (!is.null(field[['default']])) {
          if (is.function(field[['default']])) {
            self$data[[field_name]] <- field[['default']]()
          } else {
            self$data[[field_name]] <- field[['default']]
          }
        }
      }
      self$data <- utils::modifyList(self$data, args)
    },
    
    
    #' @description Insert this record into the database.
    #' @return Invisible NULL
    create = function() {
      con <- self$model$get_connection()
      
      required_fields <- names(self$model$fields)[
        vapply(self$model$fields, function(x) isFALSE(x$nullable), logical(1))
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
    #' @param ... Named arguments corresponding to field values to update.
    #' @param .data A named list of field values to update (alternative to ...).
    #' @return The Record instance (invisibly).
    update = function(..., .data = list()) {
      con <- self$model$get_connection()
      
      # Combine ... and .data, with ... taking precedence
      update_data <- utils::modifyList(.data, rlang::list2(...))
      
      # If no update data provided, use the current record data
      if (length(update_data) == 0) {
        update_data <- self$data
      } else {
        # Update the record's data with the new values
        self$data <- utils::modifyList(self$data, update_data)
      }
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
      
      non_key_fields <- setdiff(names(update_data), key_fields)
      if (length(non_key_fields) == 0) {
        stop("No non-key fields to update.")
      }
      
      set_clause <- paste0(
        non_key_fields, " = ",
        sapply(update_data[non_key_fields], DBI::dbQuoteLiteral, conn = con),
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
      invisible(self)
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
    
    #' @description
    #' Refresh this record from the database.
    #' @return The Record instance (invisibly).
    #' @details
    refresh = function() {
      key_fields <- names(self$model$fields)[
        vapply(self$model$fields, function(x) isTRUE(x$primary_key), logical(1))
      ]
      
      if (length(key_fields) == 0) {
        stop("No primary key fields defined in model.")
      }
      
      # Create key_args as a list of expressions
      key_args <- lapply(key_fields, function(field) {
        rlang::expr(!!rlang::sym(field) == !!self$data[[field]])
      })
      
      # Use do.call to construct the read call with multiple arguments
      refreshed_record <- do.call(
        self$model$read,
        c(key_args, list(mode = 'get'))
      )
      
      if (is.null(refreshed_record)) {
        stop("Record not found in database.")
      }
      self$data <- refreshed_record$data
      self$relationships <- refreshed_record$relationships
      invisible(self)
    },
    
    
    #' @description
    #' Retrieve related records based on a defined relationship.
    #'
    #' @details
    #' This method returns related records based on the relationship type:
    #' - For 'one_to_one' and 'many_to_one' relationships, it returns a single Record object or NULL.
    #' - For 'one_to_many' and 'many_to_many' relationships, it returns a list of Record objects.
    #'
    #' @param rel_name The name of the relationship to query.
    #' @param ... Additional arguments passed to the related model's read method.
    #'
    #' @return A single Record, a list of Records, or NULL, depending on the relationship type.
    relationship = function(rel_name, ...) {
      if (!rel_name %in% names(self$relationships)) stop("Invalid relationship name: ", rel_name)
      
      rel <- self$relationships[[rel_name]]
      if (!inherits(rel, "Relationship")) stop("Invalid relationship: ", rel_name)
      
      mode <- switch(rel$type,
        "belongs_to" = "one_or_none",
        "owns" = "one_or_none",
        "one_to_one" = "one_or_none",
        "one_to_many" = "all",
        "many_to_many" = "all",
        "many_to_one" = "one_or_none",
        stop("Unknown relationship type: ", rel$type)
      )
      
      # Create the foreign key filter using %in%
      fk_filter <- rlang::expr(!!rlang::sym(rel$related_key) %in% !!self$data[[rel$local_key]])
      
      # Combine the foreign key filter with any additional filters
      additional_filters <- rlang::enquos(...)
      if (length(additional_filters) > 0) {
        filters <- rlang::expr(!!fk_filter & !!!additional_filters)
      } else {
        filters <- fk_filter
      }
      
      # Use the combined filters in the read method
      result <- rel$related_model$read(!!filters, mode = mode)
      
      # Ensure we return a list for one-to-many and many-to-many relationships
      if (mode == "all" && !is.null(result)) {
        if (!is.list(result)) {
          result <- list(result)
        }
      }
      
      result
    },
    
    
    print = function() {
      cat("<Record>: '", self$model$tablename, "'\n", sep='')
      cat(paste(names(self$data), self$data, sep = ": ", collapse = "\n"), "\n")
      invisible(self)
    }
  )
)
