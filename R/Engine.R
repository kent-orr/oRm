#' Engine Class
#'
#' @description
#' The Engine class manages database connections and provides methods for
#' interacting with the database.
#' 
#' @include TableModel.R
#'
#' @export
Engine <- R6::R6Class(
  "Engine",
  public = list(
    #' @field conn_args List of connection arguments
    conn_args = NULL,

    #' @field conn Active database connection
    conn = NULL,

    #' @description
    #' Initialize a new Engine instance
    #' @param ... Connection arguments passed directly
    #' @param conn_args List of connection arguments
    initialize = function(..., conn_args = list()) {
      dots <- list(...)
      if (length(dots) > 0) {
        self$conn_args <- dots
      } else {
        self$conn_args <- conn_args
      }
    },

    #' @description
    #' Get or create a database connection
    #' @param ... Additional connection arguments
    #' @return A DBI connection object
    get_connection = function(...) {
      if (is.null(self$conn) || !DBI::dbIsValid(self$conn)) {
        args <- modifyList(self$conn_args, list(...))
        self$conn <-
          do.call(DBI::dbConnect, args)
      }
      self$conn
    },

    #' @description
    #' Close the database connection
    close = function() {
      if (!is.null(self$conn) && DBI::dbIsValid(self$conn)) {
        DBI::dbDisconnect(self$conn)
        self$conn <- NULL
      }
    },

    #' @description
    #' List all tables in the database
    #' @return A character vector of table names
    list_tables = function() {
      con <- self$get_connection()
      DBI::dbListTables(con)
    },

    #' @description
    #' Execute a SQL query and return results
    #' @param sql SQL query string
    #' @param ... Additional arguments passed to get_connection
    #' @return A data frame containing query results
    get_query = function(sql, ...) {
      con <- self$get_connection(...)
      DBI::dbGetQuery(con, sql)
    },

    #' @description
    #' Execute a SQL statement
    #' @param sql SQL statement string
    #' @param ... Additional arguments passed to get_connection
    #' @return The number of rows affected
    execute = function(sql, ...) {
      con <- self$get_connection(...)
      DBI::dbExecute(con, sql)
    },

    #' @description
    #' Create a new TableModel instance for a given table
    #' @param tablename Name of the table
    #' @param ... Additional arguments passed to TableModel$new()
    #' @return A new TableModel instance
    model = function(tablename, ...) {
      TableModel$new(tablename = tablename, engine = self, ...)
    }
  )

)
