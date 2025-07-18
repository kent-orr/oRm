#' @include TableModel.R
#' @include Dialect.R
NULL

#' Engine Class
#'
#' @description
#' The Engine class is a core component of the oRm framework, responsible for managing
#' database connections and providing methods for interacting with the database.
#' It supports both direct connections and connection pooling, offering flexibility
#' in how database resources are managed.
#'
#' Key features:
#' \itemize{
#'   \item Manages database connections (single or pooled)
#'   \item Provides methods for executing SQL queries and commands
#'   \item Allows creation of TableModel objects for ORM operations
#'   \item Supports persistent connections for improved performance
#' }
#'
#' @importFrom pool dbPool poolClose
#' @importFrom DBI dbConnect dbDisconnect dbIsValid dbListTables dbGetQuery dbExecute
#' @importFrom utils modifyList
#' @importFrom rlang list2
#'
#' @export
#'
Engine <- R6::R6Class(
  "Engine",
  public = list(
    conn_args = NULL,
    conn = NULL,
    use_pool = FALSE,
    persist = FALSE,
    dialect = NULL,



    #' @description
    #' Create an Engine object
    #' @param ... Additional arguments to be passed to DBI::dbConnect
    #' @param conn_args A list of arguments to be passed to DBI::dbConnect
    #' @param use_pool Logical. Whether or not to make use of the pool package for connections to this engine
    #' @param persist Logical. Whether to keep the connection open after operations (default: FALSE)
    initialize = function(..., conn_args = list(), use_pool = FALSE, persist = FALSE) {
      # Combine dots and conn_args, with dots taking precedence
      self$conn_args <- utils::modifyList(conn_args, rlang::list2(...))
      private$detect_dialect()
      self$use_pool <- use_pool
      self$persist <- persist
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

    #' @description
    #' Close the database connection or pool
    #' @return NULL
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

    #' @description
    #' List tables in the database connection
    #' @return A character vector of table names
    list_tables = function() {
      on.exit(if (private$exit_check()) self$close())
      DBI::dbListTables(self$get_connection())
    },


    #' Execute a SQL query and return the result as a data.frame
    #'
    #' @param sql SQL query
    #' @return A data.frame
    get_query = function(sql) {
      on.exit(if (private$exit_check()) self$close())
      DBI::dbGetQuery(self$get_connection(), sql)
    },

    #' Execute a SQL query and return the number of rows affected
    execute = function(sql) {
      on.exit(if (private$exit_check()) self$close())
      DBI::dbExecute(self$get_connection(), sql)
    },


    #' @description
    #' Create a new TableModel object for the specified table
    #' @param tablename Name of the table
    #' @param ... Additional arguments passed to the TableModel constructor
    #' @param .data A named list of the arguments for the TableModel constructor
    #' @return A new TableModel object
    model = function(tablename, ..., .data=list()) {
      TableModel$new(tablename = tablename, engine = self, ..., .data=.data)
    },

    set_transaction_state = function(state) {
      private$in_transaction <- state
    },

    get_transaction_state = function() {
      private$in_transaction
    }

  ),
  private = list(
    in_transaction = FALSE,

    exit_check = function() {
      !private$in_transaction && !self$persist && !self$use_pool
    },

    detect_dialect = function() {
      drv_name <- class(self$conn_args[['drv']])[1]
      if (grepl("Postgres|PqDriver", drv_name, ignore.case = TRUE)) {
        self$dialect <- "postgres"
      } else if (grepl("MariaDB|MySQL", drv_name, ignore.case = TRUE)) {
        self$dialect <- "mysql"
      } else if (grepl("SQLite", drv_name, ignore.case = TRUE)) {
        self$dialect <- "sqlite"
      } else {
        self$dialect <- "default"
      }
    }
  )
)

#' Transaction Function
#'
#' @description
#' This function allows you to execute a block of code within a transaction.
#' If auto_commit is TRUE (default), the transaction will be committed automatically upon successful execution.
#' If auto_commit is FALSE, you must explicitly commit or rollback within the transaction block.
#'
#' @param engine An Engine object that manages the database connection
#' @param expr An expression to be evaluated within the transaction
#' @param auto_commit Logical. Whether to automatically commit if no errors occur (default: TRUE)
#'
#' @return The result of evaluating the expression
#'
#' @examples
#' \dontrun{
#' # With auto-commit (default)
#' with.Engine(engine, {
#'   User$record(name = "Alice")$create()
#'   User$record(name = "Bob")$create()
#' })
#'
#' # With manual commit
#' with.Engine(engine, {
#'   User$record(name = "Alice")$create()
#'   User$record(name = "Bob")$create()
#'   
#'   # Explicitly commit the transaction
#'   commit()
#' }, auto_commit = FALSE)
#' }
#'
#' @export
with.Engine <- function(engine, expr, auto_commit = TRUE) {
  # Open a connection
  engine$set_transaction_state(TRUE)
  conn <- engine$get_connection()
  
  # Begin transaction
  DBI::dbBegin(conn)
  
  # Create a transaction environment with commit/rollback functions
  tx_env <- new.env(parent = parent.frame())
  tx_env$committed <- FALSE
  tx_env$rolled_back <- FALSE
  
  # Add transaction functions to the environment
  tx_env$commit <- function() {
      DBI::dbCommit(conn)
      tx_env$committed <- TRUE
      invisible(NULL)
  }
  
  tx_env$rollback <- function() {
      DBI::dbRollback(conn)
      tx_env$rolled_back <- TRUE
      invisible(NULL)
  }
  
  result <- NULL
  # Execute the expression within the transaction
  tryCatch({
      # Evaluate the expression in the transaction environment
      result <- eval(substitute(expr), tx_env)
      
      # Auto-commit if requested and not already committed/rolled back
      if (auto_commit && !tx_env$committed && !tx_env$rolled_back) {
          DBI::dbCommit(conn)
      } else if (!auto_commit && !tx_env$committed && !tx_env$rolled_back) {
          warning("Transaction was neither committed nor rolled back. Rolling back by default.")
          DBI::dbRollback(conn)
      }
      
      # Return the result
      return(result)
      
  }, error = function(e) {
      # Roll back if not already committed/rolled back
      if (!tx_env$committed && !tx_env$rolled_back) {
          DBI::dbRollback(conn)
          warning("Transaction failed, rolling back: ", e$message)
      }
      
      # Re-throw the error
      stop(e)
      
  }, finally = {
      # Clean up
      if (!engine$persist) {
          engine$close()
      }
      engine$set_transaction_state(FALSE)
  })
}
