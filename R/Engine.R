#' Engine Class
#'
#' @description
#' The Engine class manages database connections and provides methods for
#' interacting with the database.
#' 
#' @include TableModel.R
#'
#' @import pool
Engine <- R6::R6Class(
  "Engine",
  public = list(
    conn_args = NULL,
    conn = NULL,
    use_pool = FALSE,

    #' Create an Engine object
    #'
    #' @param ... args to be passed to DBI::dbConnect
    #' @param conn_args args to be passed to DBI::dbConnect
    #' @param use_pool whether or not to make use of the pool package for ceonncetions to this engine
    #' 
    initialize = function(..., conn_args = list(), use_pool = FALSE) {
      dots <- list(...)
      if (length(dots) > 0) {
        self$conn_args <- dots
      } else {
        self$conn_args <- conn_args
      }
      self$use_pool <- use_pool
    },

    #' Get a connection to the database
    #'
    #' @return A DBIConnection object or a pool object
    get_connection = function() {
      if (is.null(self$conn)) {
        if (self$use_pool) {
          self$conn <- do.call(pool::dbPool, self$conn_args)
        } else {
          self$conn <- do.call(DBI::dbConnect, self$conn_args)
        }
      }
      self$conn
    },

    #' Close the database connection or pool
    close = function() {
      if (!is.null(self$conn)) {
        if (self$use_pool) {
          pool::poolClose(self$conn)
        } else if (DBI::dbIsValid(self$conn)) {
          DBI::dbDisconnect(self$conn)
        }
        self$conn <- NULL
      }
    },

    #' list tables in the database connection
    list_tables = function() {
      on.exit(if (!self$use_pool) self$close())
      DBI::dbListTables(self$get_connection())
    },

    #' Execute a SQL query and return the result as a data.frame
    #'
    #' @param sql SQL query
    #' @return A data.frame
    get_query = function(sql) {
      on.exit(if (!self$use_pool) self$close())
      DBI::dbGetQuery(self$get_connection(), sql)
    },

    #' Execute a SQL query and return the number of rows affected
    execute = function(sql) {
      on.exit(if (!self$use_pool) self$close())
      DBI::dbExecute(self$get_connection(), sql)
    },

    #' Create a new TableModel object for the specified table
    #'
    #' @param tablename Name of the table
    #' @param ... Additional arguments passed to the TableModel constructor
    #' @param .data a named list of the arguments for the TableModel constructor
    model = function(tablename, ..., .data=list()) {
      TableModel$new(tablename = tablename, engine = self, ..., .data=.data)
    }
  )
)

