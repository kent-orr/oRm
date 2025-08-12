#' @include Dialect.R
#' @name dialect_sqlite
NULL


flush.sqlite <- function(x, table, data, con, commit = TRUE, ...) {

    # Filter out NULL values like in postgres implementation
    data <- data[!vapply(data, is.null, logical(1))]

    tbl_expr <- dbplyr::ident_q(table)
    fields <- names(data)
    if (length(fields) == 0) {
        sql <- paste0("INSERT INTO ", tbl_expr, " DEFAULT VALUES")
    } else {
        values <- sapply(data, function(val) if (is.function(val)) val() else val)
        values_sql <- paste0(
            "(", paste(DBI::dbQuoteLiteral(con, values), collapse = ", "), ")"
        )
        field_sql <- paste(DBI::dbQuoteIdentifier(con, fields), collapse = ", ")
        sql <- paste0(
            "INSERT INTO ", tbl_expr, " (", field_sql, ") VALUES ", values_sql
        )
    }
    DBI::dbExecute(con, sql)

    # Get the last inserted ID
    id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid();")[[1]]

    # Now fetch the complete row data to match postgres behavior
    # which returns all columns including defaults and auto-generated values
    result_sql <- paste0(
        "SELECT * FROM ", tbl_expr, " WHERE rowid = ", id
    )
    result <- DBI::dbGetQuery(con, result_sql)

    # Return the full row data instead of just the ID
    # This matches postgres behavior of returning all columns
    return(result)
}


qualify.sqlite <- function(x, tablename, schema) {
  if (!is.null(schema)) {
    warning("SQLite does not support schema qualification. Ignoring schema.")
  }
  tablename
}

set_schema.sqlite <- function(x, schema) {
  if (!is.null(schema)) {
    warning("SQLite does not support schemas. Ignoring set_schema().")
  }
  invisible(NULL)
}

ensure_schema_exists.sqlite <- function(x, schema) {
  invisible(NULL)

}

#' @title Apply schema prefix to SQLite table names
#' @description For SQLite, schemas are not supported natively. Any provided
#'   schema name is treated as part of the table name, acting only as a naming
#'   convention.
#' @export
set_schema.sqlite <- function(table, schema, dialect) {
  if (is.null(schema) || schema == "") {
    table
  } else {
    paste0(schema, ".", table)
  }
}

