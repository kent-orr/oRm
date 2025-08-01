#' include Dialect.R
NULL

#' TableModel Class
#'
#' @description
#' The TableModel class represents a database table in the oRm framework. It manages
#' table structure, fields, relationships, and provides methods for interacting
#' with the database table.
#'
#' @details
#' TableModel is a core component of the oRm framework, responsible for:
#' \itemize{
#'   \item Defining table structure with columns and relationships
#'   \item Creating and managing database tables
#'   \item Providing an interface for CRUD operations on table records
#'   \item Managing relationships between different tables
#' }
#'
#' Key features:
#' \itemize{
#'   \item Dynamic table creation and management
#'   \item Support for various column types and constraints
#'   \item Relationship definitions and querying
#'   \item Record creation and retrieval
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(tablename, engine, ..., .data = list())}}{Constructor for creating a new TableModel instance.}
#'   \item{\code{get_connection()}}{Retrieve the active database connection from the engine.}
#'   \item{\code{generate_sql_fields()}}{Generate SQL field definitions for table creation.}
#'   \item{\code{create_table(if_not_exists = TRUE, overwrite = FALSE, verbose = FALSE)}}{Create the associated table in the database.}
#'   \item{\code{record(..., .data = list())}}{Create a new Record object associated with this model.}
#'   \item{\code{read(..., mode = c("all", "one_or_none", "get"), limit = NULL)}}{Read records from the table using dynamic filters.}
#'   \item{\code{relationship(rel_name, ...)}}{Query related records based on defined relationships.}
#'   \item{\code{print()}}{Print a formatted overview of the model, including its fields.}
#' }
#'
#' @seealso \code{\link{Engine}}, \code{\link{Record}}, \code{\link{Column}}, \code{\link{ForeignKey}}
#'
#' @importFrom crayon silver magenta
#' @importFrom R6 R6Class
#' @importFrom dplyr tbl filter slice_head slice_tail collect
#' @importFrom rlang enquos
#' @importFrom DBI dbQuoteIdentifier dbQuoteLiteral dbExecute
#'
#' @export
#'
TableModel <- R6::R6Class(
  "TableModel",
  public = list(
    tablename = NULL,
    engine = NULL,
    fields = list(),
    relationships = list(),

    #' @description
    #' Constructor for a new TableModel.
    #' @param tablename The name of the database table.
    #' @param engine The Engine object for database connection.
    #' @param ... Column definitions.
    #' @param .data a list of Column defintions
    initialize = function(tablename, engine, ..., .data = list()) {
      if (missing(tablename) || missing(engine)) {
        stop("Both 'tablename' and 'engine' must be provided to TableModel.")
      }

      self$tablename <- tablename
      self$engine <- engine

      dots <- utils::modifyList(.data, rlang::list2(...))
      col_defs <- dots[vapply(dots, inherits, logical(1), "Column")]
      col_names <- names(dots[vapply(dots, inherits, logical(1), "Column")])
      for (i in seq_along(col_defs)) {
        col_defs[[i]][['name']] <- col_names[i]
        class(col_defs[[i]]) <- append(class(col_defs[[i]]), engine$dialect)
      }
      self$fields <- col_defs
    },

    #' @description
    #' Retrieve the active database connection from the engine.
    get_connection = function(...) {
      self$engine$get_connection(...)
    },


    #' @description
    #' Create the associated table in the database.
    #' @param if_not_exists Logical. If TRUE, only create the table if it doesn't exist. Default is TRUE.
    #' @param overwrite Logical. If TRUE, drop the table if it exists and recreate it. Default is FALSE.
    #' @param verbose Logical. If TRUE, return the SQL statement instead of executing it. Default is FALSE.
    #'
    create_table = function(if_not_exists = TRUE, overwrite = FALSE, verbose = FALSE) {
      conn <- self$get_connection()

      if (overwrite) {
        drop_sql <- paste0("DROP TABLE IF EXISTS ", DBI::dbQuoteIdentifier(conn, self$tablename))
        if (verbose) {
          cat(drop_sql, "\n")
        } else {
          DBI::dbExecute(conn, drop_sql)
        }
      }
      fields_sql = c()
      constraints_sql = c()
      for (i in seq_along(self$fields)) {
        field_name = names(self$fields)[i]
        field = self$fields[[i]]
        fields_sql = c(fields_sql, render_field(field, conn))
        constraints_sql = c(constraints_sql, render_constraint(field, conn))
      }

      create_clause <- if (if_not_exists) "CREATE TABLE IF NOT EXISTS" else "CREATE TABLE"
      sql <- paste0(
        create_clause, " ",
        DBI::dbQuoteIdentifier(conn, self$tablename),
        " (\n  ", paste(fields_sql, collapse = ',\n'), 
        if (length(constraints_sql) > 0 && any(constraints_sql != "")) {
          paste0(",\n  ", paste(constraints_sql[constraints_sql != ""], collapse = ",\n  "))
        } else {
          ""
        },
        "\n);\n"
      )

      if (verbose) {
        return(sql)
      }

      DBI::dbExecute(conn, sql)
      return(self)
    },

    #' @description
    #' Drop the associated table from the database. Prompts for confirmation
    #' by default if running interactively.
    #'
    #' @param ask Logical. If TRUE (default in interactive sessions), prompts
    #' the user for confirmation before dropping the table.
    #'
    #' @return Invisibly returns the result of `DBI::dbExecute()` if the table is dropped,
    #' or `NULL` if the operation is canceled or skipped.
    #'
    #' @examples
    #' \dontrun{
    #' # Drop the "users" table after confirmation
    #' User$drop_table()
    #'
    #' # Force drop without confirmation
    #' User$drop_table(ask = FALSE)
    #' }
    drop_table = function(ask = interactive()) {
      con <- self$get_connection()
      drop_sql <- paste0("DROP TABLE IF EXISTS ", DBI::dbQuoteIdentifier(con, self$tablename))

      resp <- 'y'
      if (ask) {
        resp <- readline(paste0("Are you sure you want to drop ", self$tablename, "? [y/N] "))
      }

      if (grepl('y', resp, ignore.case = TRUE)) {
        DBI::dbExecute(con, drop_sql)
      } else {
        message("Table not dropped.")
        return(invisible(NULL))
      }
    },


    #' @description
    #' Create a new Record object with this model.
    #' @param ... Named values to initialize the record's data.
    #' @param .data a named list of field values.
    #'
    record = function(..., .data = list()) {
      Record$new(self, ..., .data = .data)
    },

    tbl = function() {
      con = self$get_connection()
      dplyr::tbl(con, self$tablename)
    },

    #' @description
    #' Read records using dynamic filters and return in the specified mode.
    #' @param ... Unquoted expressions for filtering.
    #' @param mode One of "all", "one_or_none", or "get".
    #' @param .limit Integer. Maximum number of records to return. Defaults to 100. NULL means no limit.
    #'   Positive values return the first N records, negative values return the last N records.
    #' @param .offset Integer. Offset for pagination. Default is 0.
    #' @param .order_by Unquoted expressions for ordering. Defaults to NULL (no order). Calls dplyr::arrange() so can take multiple args / desc()
    read = function(
      ..., 
      mode = c("all", "one_or_none", "get"), 
      .limit = 100, 
      .offset=0,
      .order_by = list()
    ) {
      
      mode <- match.arg(mode)
      tbl_ref <- self$tbl()

      filters <- rlang::enquos(...)
      if (length(filters) > 0) {
        tbl_ref <- dplyr::filter(tbl_ref, !!!filters)
      }

      # Apply ordering (only if user provided .order_by)
      if (!missing(.order_by)) {
        order_exprs <- rlang::enexpr(.order_by)
      
        # Handle .order_by = c(x, desc(y)) vs .order_by = x
        if (rlang::is_call(order_exprs, "c")) {
          order_exprs <- as.list(order_exprs)[-1]  # Drop the "c" call
        } else {
          order_exprs <- list(order_exprs)
        }
      
        tbl_ref <- dplyr::arrange(tbl_ref, !!!order_exprs)
      }

      # Apply pagination using SQL-compatible operations
      if (!is.null(.limit) && is.numeric(.limit) && .limit != 0) {
        if (.limit > 0) {
          # For positive limits with offset
          if (.offset > 0) {
            # SQL databases support LIMIT and OFFSET directly
            tbl_ref <- tbl_ref |> 
              dplyr::mutate(.row_id = dplyr::row_number()) |>
              dplyr::filter(.row_id > .offset & .row_id <= .offset + .limit) |>
              dplyr::select(-.row_id)
          } else {
            # Just limit without offset
            tbl_ref <- tbl_ref |>
              dplyr::mutate(.row_id = dplyr::row_number()) |>
              dplyr::filter(.row_id <= .limit) |>
              dplyr::select(-.row_id)
          }
        } else {
          # For negative limits (last N rows)
          tbl_ref <- tbl_ref |>
            dplyr::mutate(.row_id = dplyr::row_number()) |>
            dplyr::filter(.row_id > dplyr::n() - abs(.limit)) |>
            dplyr::select(-.row_id)
        }
      } else if (.offset > 0) {
        # Just offset without limit
        tbl_ref <- tbl_ref |>
          dplyr::mutate(.row_id = dplyr::row_number()) |>
          dplyr::filter(.row_id > .offset) |>
          dplyr::select(-.row_id)
      }
      
      rows <- dplyr::collect(tbl_ref)

      if (nrow(rows) == 0) {
        if (mode == "get") {
          stop("Expected exactly one row, got: 0")
        } else if (mode == "one_or_none" || mode == "all") {
          return(NULL)
        }
      }
      create_record <- function(row_data) {
        Record$new(model = self, .data = as.list(row_data))
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

      # mode == "all"
      lapply(seq_len(nrow(rows)), function(i) create_record(rows[i, , drop = TRUE]))
    },

    #' @description
    #' Query related records based on defined relationships.
    #' @param rel_name The name of the relationship to query.
    #' @param ... Additional arguments passed to the related model's read method.
    #' @return A list of related records or a single record, depending on the relationship type.
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

      rel$related_model$read(..., mode=mode)

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
        nullable = vapply(self$fields, function(x) {
          if (is.null(x$nullable)) NA else x$nullable
        }, logical(1)),
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
        "date"      = magenta:70
        (x),
        "timestamp" = magenta(x),
        silver(x))
      }

      for (i in seq_len(nrow(field_df))) {
        row <- field_df[i, ]
        key_icon <- if (row$key) "🔑" else "  "
        name_str <- format(row$name, width = col_widths$name)
        type_raw <- format(row$type, width = col_widths$type)
        type_str <- color_type(type_raw)
        null_str <- if (is.na(row$nullable)) "" else if (row$nullable) "NULL" else "NOT NULL"

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
