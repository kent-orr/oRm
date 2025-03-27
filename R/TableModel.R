#' Define a database column for a model
#'
#' Creates a `Column` object describing the type and constraints for a field
#' in a table model. Intended for use within `TableModel` or `TableModel` definitions.
#'
#' @param type Character string representing the column's SQL data type
#'   (e.g. `"INTEGER"`, `"TEXT"`, `"VARCHAR"`).
#' @param default The default value to use if none is supplied when creating a record.
#'   If `NULL`, no default will be applied at the SQL level.
#' @param primary_key Logical. Whether this field is part of the primary key.
#'   Defaults to `FALSE`.
#' @param nullable Logical. Whether the column may accept `NULL` values.
#'   Defaults to `TRUE`.
#' @param unique Logical. Whether the column should be constrained as `UNIQUE`.
#'   Defaults to `FALSE`. Currently unused in table creation.
#' @param foreign_key Optional character string specifying a foreign key reference
#'   in the format `"referenced_table.referenced_column"`. Currently used for SQL generation.
#' @param on_delete Character string indicating `ON DELETE` behavior (e.g., `"CASCADE"`, `"SET NULL"`).
#'   Only relevant if `foreign_key` is set. Optional.
#' @param on_update Character string indicating `ON UPDATE` behavior (e.g., `"CASCADE"`, `"RESTRICT"`).
#'   Only relevant if `foreign_key` is set. Optional.
#' @param ... Reserved for future extensions, such as `check`, `collate`, or custom constraints.
#'
#' @return An object of class `"Column"`, used to define fields in a TableModel.
#'
#' @examples
#' Column("TEXT", nullable = FALSE)
#' Column("INTEGER", primary_key = TRUE)
#' Column("INTEGER", foreign_key = "users.id", on_delete = "CASCADE")
#' 
#' @importFrom crayon silver magenta
#'
#' @export
Column <- function(
  type, 
  default = NULL, 
  primary_key = FALSE, 
  nullable = TRUE, 
  unique = FALSE, 
  foreign_key = NULL, 
  on_delete = NULL, 
  on_update = NULL, 
  ...) {
    
    structure(
      # Internal structure with extended FK support
      list(
        type = type,
        nullable = nullable,
        primary_key = primary_key,
        default = default,
        nullable = nullable,
        unique = unique,
        foreign_key = foreign_key,
        on_delete = on_delete,
        on_update = on_update,
        extras = list(...)
      ),
      class = "Column"
    )
  }
  
  TableModel <- R6::R6Class(
    "TableModel",
    public = list(
      tablename = NULL,
      engine = NULL,
      fields = list(),
      
      #' @description
      #' Constructor for a new TableModel.
      #' @param tablename The name of the database table.
      #' @param engine The Engine object for database connection.
      #' @param ... Column definitions.
      initialize = function(tablename, engine, ...) {
        if (missing(tablename) || missing(engine)) {
          stop("Both 'tablename' and 'engine' must be provided to TableModel.")
        }
        
        self$tablename <- tablename
        self$engine <- engine
        
        dots <- list(...)
        col_defs <- dots[vapply(dots, inherits, logical(1), "Column")]
        self$fields <- col_defs
      },
      
      #' @description
      #' Retrieve the active database connection from the engine.
      get_connection = function(...) {
        self$engine$get_connection(...)
      },
      
      #' @description
      #' Generate DBI-compatible field definitions from Column objects, including type,
      #' nullable constraint, and default values.
      generate_sql_fields = function() {
        out <- list()
        constraints <- list()
        con <- self$get_connection()
        
        for (name in names(self$fields)) {
          col <- self$fields[[name]]
          parts <- c(DBI::dbQuoteIdentifier(con, name), col$type)
          
          if (!isTRUE(col$nullable)) {
            parts <- c(parts, "NOT NULL")
          }
          
          if (!is.null(col$default)) {
            parts <- c(parts, "DEFAULT", DBI::dbQuoteLiteral(con, col$default))
          }
          
          if (isTRUE(col$unique)) {
            parts <- c(parts, "UNIQUE")
          }
          
          if (isTRUE(col$primary_key)) {
            parts <- c(parts, "PRIMARY KEY")
          }
          
          if (!is.null(col$extras) && length(col$extras) > 0) {
            parts <- c(parts, unlist(col$extras))
          }
          
          field_sql <- paste(parts, collapse = " ")
          out[[name]] <- field_sql
          
          if (!is.null(col$foreign_key)) {
            fk_parts <- paste0(
              "FOREIGN KEY (", DBI::dbQuoteIdentifier(con, name), ") REFERENCES ",
              col$foreign_key
            )
            
            if (!is.null(col$on_delete)) {
              fk_parts <- paste(fk_parts, "ON DELETE", toupper(col$on_delete))
            }
            
            if (!is.null(col$on_update)) {
              fk_parts <- paste(fk_parts, "ON UPDATE", toupper(col$on_update))
            }
            
            constraints[[length(constraints) + 1]] <- fk_parts
          }
        }
        
        # Append constraints as additional definitions
        if (length(constraints)) {
          for (i in seq_along(constraints)) {
            out[[paste0("__constraint", i)]] <- constraints[[i]]
          }
        }
        
        out
      },
      
      #' @description
      #' Create the associated table in the database.
      #' @description
      #' Create the associated table in the database.
      #' @param if_not_exists Logical. If TRUE, only create the table if it doesn't exist. Default is TRUE.
      #' @param overwrite Logical. If TRUE, drop the table if it exists and recreate it. Default is FALSE.
      #' @param verbose Logical. If TRUE, return the SQL statement instead of executing it. Default is FALSE.
      create_table = function(if_not_exists = TRUE, overwrite = FALSE, verbose = FALSE) {
        con <- self$get_connection()

        if (overwrite) {
          drop_sql <- paste0("DROP TABLE IF EXISTS ", DBI::dbQuoteIdentifier(con, self$tablename))
          if (verbose) {
            cat(drop_sql, "\n")
          } else {
            DBI::dbExecute(con, drop_sql)
          }
        }
        fields_sql <- self$generate_sql_fields()
        field_defs <- paste(unname(fields_sql), collapse = ",\n  ")

        create_clause <- if (if_not_exists) "CREATE TABLE IF NOT EXISTS" else "CREATE TABLE"
        sql <- paste0(
          create_clause, " ", 
          DBI::dbQuoteIdentifier(con, self$tablename), 
          " (\n  ", field_defs, "\n)"
        )

        if (verbose) {
          return(sql)
        }

        DBI::dbExecute(con, sql)
      }
      
      #' @description
      #' Create a new Record object with this model.
      #' @param ... Named values to initialize the record's data.
      record = function(...) {
        Record$new(self, data = list(...))
      },
      
      #' @description
      #' Read records using dynamic filters and return in the specified mode.
      #' @param ... Unquoted expressions for filtering.
      #' @param mode One of "all", "one_or_none", or "get".
      read = function(..., mode = c("all", "one_or_none", "get")) {
        mode <- match.arg(mode)
        con <- self$get_connection()
        tbl_ref <- dplyr::tbl(con, self$tablename)
        
        filters <- rlang::enquos(...)
        if (length(filters) > 0) {
          tbl_ref <- dplyr::filter(tbl_ref, !!!filters)
        }
        
        rows <- dplyr::collect(tbl_ref)
        create_record <- function(row_data) {
          Record$new(model = self, data = as.list(row_data))
        }
        
        if (mode == "get") {
          if (nrow(rows) != 1) stop("Expected exactly one row, got: ", nrow(rows))
          return(create_record(rows[1, , drop = TRUE]))
        }
        
        if (mode == "one_or_none") {
          if (nrow(rows) > 1) stop("Expected zero or one row, got multiple")
          if (nrow(rows) == 1) return(create_record(rows[1, , drop = TRUE]))
          return(NULL)
        }
        
        lapply(seq_len(nrow(rows)), function(i) create_record(rows[i, , drop = TRUE]))
      },
      
      #' @description
      #' Delete rows from the table based on filter expressions.
      #' @param ... Unquoted dplyr-style filters.
      delete_where = function(..., verbose = FALSE) {
        con <- self$get_connection()
        
        tbl_filtered <- dplyr::tbl(con, self$tablename) %>% dplyr::filter(...)
        sql_query <- dbplyr::sql_build(tbl_filtered)
        where_expr <- sql_query$where
        
        if (is.null(where_expr)) stop("No WHERE clause generated — refusing to delete everything.")
        
        sql <- dbplyr::build_sql(
          "DELETE FROM ", DBI::dbQuoteIdentifier(con, self$tablename),
          " WHERE ", where_expr,
          con = con
        )
        
        DBI::dbExecute(con, sql)
      },
      
      #' @description
      #' Print a formatted overview of the model, including its fields.
      print = function(...) {
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
          key = vapply(self$fields, function(x) isTRUE(x$primary_key), logical(1)),
          stringsAsFactors = FALSE
        )
        
        field_df <- field_df[order(-field_df$key), ]
        n_display <- min(10, nrow(field_df))
        field_df <- field_df[seq_len(n_display), ]
        
        col_widths <- list(
          name = max(10, max(nchar(field_df$name))),
          type = max(8, max(nchar(field_df$type))),
          null = 9
        )
        
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
          type_raw <- format(row$type, width = col_widths$type)
          type_str <- color_type(type_raw)
          null_str <- if (row$nullable) "NULL" else "NOT NULL"
          
          cat(sprintf("  %s %s  %s  %s\n", key_icon, name_str, type_str, null_str))
        }
        
        if (length(self$fields) > n_display) {
          cat(silver(sprintf("  ... %d more columns not shown\n",
          length(self$fields) - n_display)))
        }
        
        invisible(self)
      }
    )
  )