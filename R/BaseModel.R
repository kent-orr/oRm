#' Base Model Class
#'
#' @description
#' The BaseModel class is the foundation for creating object-relational mappings in oRm.
#' It provides methods for table creation, record manipulation, and querying.
#'
#' @details
#' BaseModel is an R6 class that represents a database table. It manages the connection
#' to the database, defines the table structure, and provides methods for interacting
#' with the table data.
#'
#' @field tablename Character. The name of the database table.
#' @field engine An Engine object. Manages the database connection.
#' @field fields List. Contains Column objects defining the table structure.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(tablename, engine, ...)}}{Constructor for creating a new BaseModel instance.}
#'   \item{\code{get_connection()}}{Returns the database connection from the engine.}
#'   \item{\code{get_fields_for_dbi()}}{Converts Column definitions to DBI-compatible format.}
#'   \item{\code{create_table()}}{Creates the table in the database.}
#'   \item{\code{print()}}{Prints a formatted representation of the model.}
#'   \item{\code{record(...)}}{Creates a new Record instance for this model.}
#'   \item{\code{delete_where(...)}}{Deletes rows from the table based on filter conditions.}
#'   \item{\code{read(..., mode)}}{Reads records from the table with optional filtering.}
#' }
#' 
#' @importFrom dplyr tbl filter collect
#' @importFrom rlang sym
#'
#' @examples
#' \dontrun{
#' # Create a new BaseModel
#' User <- BaseModel$new(
#'   tablename = "users",
#'   engine = my_engine,
#'   id = Column("INTEGER", key = TRUE),
#'   name = Column("VARCHAR")
#' )
#'
#' # Create the table in the database
#' User$create_table()
#'
#' # Insert a new record
#' User$record(id = 1, name = "Alice")$create()
#'
#' # Read records
#' all_users <- User$read()
#' alice <- User$read(name == "Alice", mode = "one_or_none")
#'
#' # Delete records
#' User$delete_where(name == "Alice")
#' }
#'
#' @export
BaseModel <- R6::R6Class(
  "BaseModel",
  public = list(
    tablename = NULL,
    engine = NULL,
    fields = list(),


    initialize = function(tablename, engine, ...) {
      if (missing(tablename) || missing(engine)) {
        stop("Both 'tablename' and 'engine' must be provided to BaseModel.")
      }

      self$tablename <- tablename
      self$engine <- engine

      dots <- list(...)
      col_defs <-
        dots[vapply(dots, inherits, logical(1), "Column")]
      self$fields <- col_defs
    },

    get_connection = function(...) {
      self$engine$get_connection(...)
    },

    get_fields_for_dbi = function() {
      # Convert Column definitions into DBI-compatible list
      vapply(self$fields, function(col)
        col$type, FUN.VALUE = character(1))
    },

    create_table = function() {
      DBI::dbCreateTable(
        self$get_connection(),
        name = self$tablename,
        fields = self$get_fields_for_dbi()
      )
    },

    #' Create a record
    #' 
    #' This method creates a new record in the associated table.
    #' It takes the record data as a named list and inserts it into the table using DBI's `dbInsertTable`.
    #' @param... Named list of values to insert into the table.
    #'   The names should match the column names in the table.
    #' @return The new record object.
    record = function(...) {
      Record$new(self, data = list(...))
    },

    #' Read records from the table using dynamic filters
    #'
    #' Uses a `dbplyr`-based query to read records from the associated table.
    #' You can specify filter conditions and return behavior, similar to SQLAlchemy.
    #'
    #' @param ... Filter expressions passed as unquoted arguments (e.g. age > 18, name == "Alice")
    #' @param mode Character string: one of `"all"`, `"one_or_none"`, or `"get"`.
    #'   - `"all"` returns a list of `Record` objects (default)
    #'   - `"one_or_none"` returns a single `Record` or `NULL`
    #'   - `"get"` returns a single `Record` and errors if more or less than one match
    #'
    #' @return One or more `Record` objects depending on the mode.
    read = function(..., mode = c("all", "one_or_none", "get")) {
      mode <- match.arg(mode)
      con <- self$get_connection()
      tbl_ref <- dplyr::tbl(con, self$tablename)
    
      # Capture expressions from ...
      filters <- rlang::enquos(...)
    
      # Apply filters if any
      if (length(filters) > 0) {
        tbl_ref <- dplyr::filter(tbl_ref, !!!filters)
      }
    
      rows <- dplyr::collect(tbl_ref)
    
      create_record <- function(row_data) {
        Record$new(model = self, data = as.list(row_data))
      }
    
      if (mode == "get") {
        if (nrow(rows) != 1) {
          stop("Expected exactly one row, got: ", nrow(rows))
        }
        return(create_record(rows[1, , drop = TRUE]))
      }
    
      if (mode == "one_or_none") {
        if (nrow(rows) > 1) {
          stop("Expected zero or one row, but got multiple")
        } else if (nrow(rows) == 1) {
          return(create_record(rows[1, , drop = TRUE]))
        } else {
          return(NULL)
        }
      }
    
      # Default: all
      lapply(seq_len(nrow(rows)), function(i) {
        create_record(rows[i, , drop = TRUE])
      })
    },    

    #' Delete rows matching a filter expression
    #'
    #' This method deletes rows from the associated table based on the provided filter expressions.
    #' It uses dplyr-style filter expressions to specify the rows to be deleted.
    #'
    #' @param ... Unquoted dplyr-style filter expressions.
    #'   These expressions should be used to filter the rows to be deleted.
    #'   For example, `age > 18` will delete all rows where the `age` column is greater than 18.
    #' @return The number of rows deleted.
    #' @examples
    #' \dontrun{
    #' # Delete all rows where the 'age' column is greater than 18
    #' my_model$delete_where(age > 18)
    #'
    #' # Delete all rows where the 'name' column is 'Alice' and 'age' is less than 30
    #' my_model$delete_where(name == "Alice", age < 30)
    #' }
    delete_where = function(...) {
      con <- self$get_connection()
    
      # Build lazy table with filter conditions
      tbl_filtered <- dplyr::tbl(con, self$tablename) %>%
        dplyr::filter(...)
    
      # Use dbplyr internals to pull WHERE clause
      sql_query <- dbplyr::sql_build(tbl_filtered)
      where_expr <- sql_query$where
    
      if (is.null(where_expr)) {
        stop("No WHERE clause generated — refusing to delete everything.")
      }
    
      sql <- dbplyr::build_sql(
        "DELETE FROM ", DBI::dbQuoteIdentifier(con, self$tablename),
        " WHERE ", where_expr,
        con = con
      )
    
      DBI::dbExecute(con, sql)
    },

    print = function(...) {
      library(crayon)

      cat("<", class(self)[1], ">\n", sep = "")
      cat("  Table: ", self$tablename, "\n", sep = "")

      if (length(self$fields) == 0) {
        cat("  Fields: (none defined)\n")
        return(invisible(self))
      }

      cat("  Fields:\n")

      field_df <- data.frame(
        name = names(self$fields),
        type = vapply(self$fields, function(x) x$type, character(1)),
        nullable = vapply(self$fields, function(x) x$nullable, logical(1)),
        key = vapply(self$fields, function(x) x$key, logical(1)),
        stringsAsFactors = FALSE
      )

      # Order key fields to top
      field_df <- field_df[order(-field_df$key), ]

      # Truncate output if needed
      n_display <- min(10, nrow(field_df))
      field_df <- field_df[seq_len(n_display), ]

      # Set fixed column widths
      col_widths <- list(
        name = max(10, max(nchar(field_df$name))),
        type = max(8, max(nchar(field_df$type))),
        null = 9
      )

      # Type coloring
      color_type <- function(x) {
        switch(tolower(x),
               "integer"   = green(x),
               "int"       = green(x),
               "numeric"   = yellow(x),
               "real"      = yellow(x),
               "text"      = blue(x),
               "varchar"   = blue(x),
               "date"      = magenta(x),
               "timestamp" = magenta(x),
               silver(x))
      }

        for (i in seq_len(nrow(field_df))) {
          row <- field_df[i, ]
          key_icon <- if (row$key) "🔑" else "  "
          name_str <- format(row$name, width = col_widths$name)

          # Step 1: format plain type to fixed width
          type_raw <- format(row$type, width = col_widths$type)
          # Step 2: apply crayon color *after* padding
          type_str <- color_type(type_raw)

          null_str <- if (row$nullable) "NULL" else "NOT NULL"

          cat(sprintf("  %s %s  %s  %s\n",
                      key_icon,
                      name_str,
                      type_str,
                      null_str))

      }

      if (length(self$fields) > n_display) {
        cat(silver(sprintf("  ... %d more columns not shown\n",
                           length(self$fields) - n_display)))
      }

      invisible(self)
    }

  )
)