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
#' @field conn_args A list of arguments for establishing a connection
#' @field conn Active database connection or pool
#' @field use_pool Whether to use connection pooling
#' @field persist Whether to keep connections open between operations
#' @field dialect Database dialect in use
#' @field schema Default schema applied to tables
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
        schema  = NULL,
        
        
        
        #' @description
        #' Create an Engine object
        #' @param ... Additional arguments to be passed to DBI::dbConnect
        #' @param conn_args A list of arguments to be passed to DBI::dbConnect
        #' @param .schema Character. The default schema to apply to child TableModel objects
        #' @param use_pool Logical. Whether or not to make use of the pool package for connections to this engine
        #' @param persist Logical. Whether to keep the connection open after operations (default: FALSE)
        initialize = function(..., conn_args = list(), .schema = NULL, use_pool = FALSE, persist = FALSE) {
            # Combine dots and conn_args, with dots taking precedence
            self$conn_args <- utils::modifyList(conn_args, rlang::list2(...))
            private$detect_dialect()
            self$schema <- .schema
            self$use_pool <- use_pool
            self$persist <- persist
        },
        
        
        
        #' Get a connection to the database
        #'
        #' Reapplies the configured schema on every connection retrieval to
        #' ensure consistency after reconnects.
        #'
        #' @return A DBIConnection object or a pool object
        get_connection = function() {
            if (is.null(self$conn) || (!self$use_pool && !DBI::dbIsValid(self$conn))) {
                if (self$use_pool) {
                    self$conn <- do.call(pool::dbPool, self$conn_args)
                } else {
                    self$conn <- do.call(DBI::dbConnect, self$conn_args)
                }
            }

            if (!is.null(self$schema) && identical(self$dialect, "postgres")) {
                sql <- paste0("SET search_path TO ", DBI::dbQuoteIdentifier(self$conn, self$schema))
                DBI::dbExecute(self$conn, sql)
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
        
        
        #' @description
        #' Execute a SQL query and return the result as a data.frame
        #' @param sql SQL query
        #' @return A data.frame
        get_query = function(sql) {
            on.exit(if (private$exit_check()) self$close())
            DBI::dbGetQuery(self$get_connection(), sql)
        },

        #' @description
        #' Execute a SQL query and return the number of rows affected
        #' @param sql SQL query
        #' @return The number of rows affected
        execute = function(sql) {
            on.exit(if (private$exit_check()) self$close())
            DBI::dbExecute(self$get_connection(), sql)
        },

        #' @description
        #' Set the default schema for the engine and active connection
        #' @param schema Character. Schema name to apply
        #' @return The Engine object
        set_schema = function(schema) {
            on.exit(if (private$exit_check()) self$close())
            self$schema <- schema
            set_schema(self, schema)
            return(self)
        },
        
        #' @description
        #' Explicitly create a schema in the database
        #' @param schema Character. The schema name to create
        #' @return TRUE (invisible) if schema created/existed
        create_schema = function(schema) {
            create_schema(self, schema)
        },
        
        #' @description
        #' Create a new TableModel object for the specified table
        #' @param tablename Name of the table
        #' @param ... Additional arguments passed to the TableModel constructor.
        #'   Include `Column` objects here to define the table structure.
        #' @param .data A named list of the arguments for the TableModel constructor.
        #'   `Column` objects in this list also define table structure.
        #' @param .schema Character. The default schema to apply to the TableModel object
        #' @param .default_mode Character. Default read mode for the TableModel.
        #' @return A new TableModel object
        #' @seealso [TableModel::new()]
        #' @examples
        #' engine$model(
        #'     "users",
        #'     Column$new("id", "integer"),
        #'     Column$new("name", "text")
        #' )
        model = function(tablename, ..., .data = list(), .schema = NULL, .default_mode = "all") {
            if (is.null(.schema)) .schema <- self$schema
            if (!is.null(.schema)) ensure_schema_exists(self, .schema)
            tablename <- qualify(self, tablename, schema = .schema)
            TableModel$new(tablename = tablename, engine = self, ..., .data = .data, .schema = .schema, .default_mode = .default_mode)
        },
        
        
        #' @description
        #' Set the internal transaction state
        #' @param state Logical. Indicates if a transaction is active
        #' @return NULL
        set_transaction_state = function(state) {
            private$in_transaction <- state
        },

        #' @description
        #' Retrieve the current transaction state
        #' @return Logical indicating if a transaction is active
        get_transaction_state = function() {
            private$in_transaction
        },

        #' @description
        #' Qualify a table name with a schema
        #' @param tablename Character. Table name to qualify
        #' @param .schema Character. Schema name to prepend
        #' @return A fully qualified table name
        qualify = function(tablename, .schema = self$schema) {
            qualify(self, tablename, schema = .schema)
        },


        #' @description
        #' Quote and format a schema-qualified table name
        #' @param tablename Character. Table name to format
        #' @return A quoted table name
        format_tablename = function(tablename) {
            parts <- strsplit(tablename, "\\.")[[1]]
            quoted <- vapply(
                parts,
                function(x) DBI::dbQuoteIdentifier(self$conn, x),
                character(1)
            )
            paste(quoted, collapse = ".")
        },

        #' @description
        #' Print a concise summary of the engine, including the SQL dialect,
        #' default schema, and connection status.
        #' @param ... Unused, present for compatibility.
        #' @return The Engine object, invisibly.
        #' @seealso [TableModel$print()], [Record$print()].
        print = function(...) {
            connected <- FALSE
            if (!is.null(self$conn)) {
                connected <- DBI::dbIsValid(self$conn)
            }
            cat("<", class(self)[1], ">\n", sep = "")
            cat(
                "  dialect: ", self$dialect,
                ", schema: ", if (is.null(self$schema)) "NULL" else self$schema,
                ", connected: ", connected, "\n",
                sep = ""
            )
            invisible(self)
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
#' @details
#' Within the transaction block, the following special functions are available:
#' 
#' \itemize{
#'   \item \code{commit()}: Explicitly commits the current transaction. After calling this function,
#'         no further changes will be made to the database within the current transaction block.
#'         
#'   \item \code{rollback()}: Explicitly rolls back (cancels) the current transaction. This undoes
#'         all changes made within the transaction block up to this point.
#' }
#'
#' If \code{auto_commit = TRUE} (the default), the transaction will be automatically committed when
#' the block completes without errors. If an error occurs, the transaction is automatically rolled back.
#'
#' If \code{auto_commit = FALSE}, you must explicitly call \code{commit()} within the block to save
#' your changes. If neither \code{commit()} nor \code{rollback()} is called, the transaction will be
#' rolled back by default and a warning will be issued.
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
#'   # Transaction automatically committed if no errors
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
#'
#' # With conditional commit/rollback
#' with.Engine(engine, {
#'   User$record(name = "Alice")$create()
#'   
#'   # Check a condition
#'   if (some_validation_check()) {
#'     User$record(name = "Bob")$create()
#'     commit()
#'   } else {
#'     # Discard all changes if validation fails
#'     rollback()
#'   }
#' }, auto_commit = FALSE)
#'
#' # Error handling with explicit rollback
#' with.Engine(engine, {
#'   tryCatch({
#'     User$record(name = "Alice")$create()
#'     # Some operation that might fail
#'     problematic_operation()
#'     commit()
#'   }, error = function(e) {
#'     # Custom error handling
#'     message("Operation failed: ", e$message)
#'     rollback()
#'   })
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

