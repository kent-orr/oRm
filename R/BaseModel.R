#' TableModel Class
#'
#' @description
#' The TableModel class is the foundation for creating object-relational mappings in oRm.
#' It provides methods for table creation, record manipulation, and querying.
#'
#' @details
#' TableModel is an R6 class that represents a database table. It manages the connection
#' to the database, defines the table structure, and provides methods for interacting
#' with the table data.
#'
#' @field tablename Character. The name of the database table.
#' @field engine An Engine object. Manages the database connection.
#' @field fields List. Contains Column objects defining the table structure.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(tablename, engine, ...)}}{Constructor for creating a new TableModel instance.}
#'   \item{\code{get_connection()}}{Returns the database connection from the engine.}
#'   \item{\code{get_fields_for_dbi()}}{Converts Column definitions to DBI-compatible format.}
#'   \item{\code{create_table()}}{Creates the table in the database.}
#'   \item{\code{print()}}{Prints a formatted representation of the model.}
#'   \item{\code{record(...)}}{Creates a new Record instance for this model.}
#'   \item{\code{read(..., mode)}}{Reads records from the table with optional filtering.}
#'   \item{\code{delete_where(...)}}{Deletes rows from the table based on filter conditions.}
#' }
#'
#' @importFrom dplyr tbl filter collect
#' @importFrom rlang enquos sym
#' @importFrom DBI dbCreateTable dbExecute dbQuoteIdentifier dbQuoteLiteral
#' @importFrom dbplyr sql_build build_sql
#' @importFrom crayon green yellow blue magenta silver
#' @export
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
    #' Generate DBI-compatible field definitions from Column objects.
    get_fields_for_dbi = function() {
      vapply(self$fields, function(col) col$type, FUN.VALUE = character(1))
    },

    #' @description
    #' Create the associated table in the database.
    create_table = function() {
      DBI::dbCreateTable(
        self$get_connection(),
        name = self$tablename,
        fields = self$get_fields_for_dbi()
      )
    },

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
    delete_where = function(...) {
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
        key = vapply(self$fields, function(x) x$key, logical(1)),
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