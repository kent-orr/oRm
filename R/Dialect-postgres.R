#' @describeIn flush Insert a row and return the inserted record using PostgreSQL's RETURNING clause.
flush.postgres <- function(x, table, data, con, commit = TRUE, ...) {
  # Build the insert SQL
  data <- data[!vapply(data, is.null, logical(1))]
  tbl_expr <- dbplyr::ident_q(table)
  fields <- names(data)

  values_sql <- paste0("(", paste(DBI::dbQuoteLiteral(con, sapply(data, `[`)), collapse = ", "), ")")
  field_sql <- paste(DBI::dbQuoteIdentifier(con, fields), collapse = ", ")

  sql <- paste0(
    "INSERT INTO ", tbl_expr, " (", field_sql, ") VALUES ", values_sql,
    " RETURNING *"
  )

  # Just execute the query and return the result
  # Let the caller handle transaction management
  result <- DBI::dbGetQuery(con, sql)
  
  return(result)
}


#' @describeIn qualify Add the schema prefix to unqualified table names for PostgreSQL.
qualify.postgres <- function(x, tablename, schema) {
  if (!grepl("\\.", tablename) && !is.null(schema)) {
    paste(schema, tablename, sep = ".")
  } else {
    tablename
  }
}

#' @describeIn set_schema PostgreSQL applies schema via search_path; updates occur during connection retrieval.
set_schema.postgres <- function(x, schema) {
    # Schema updates are handled during connection retrieval
    invisible(NULL)
}

#' @describeIn ensure_schema_exists Create the schema with CREATE SCHEMA IF NOT EXISTS for PostgreSQL databases.
ensure_schema_exists.postgres <- function(x, schema) {
  if (is.null(schema)) return(invisible(NULL))

  conn <- NULL
  if (inherits(x, "Engine")) {
    conn <- x$conn
    if (is.null(conn) || !DBI::dbIsValid(conn)) {
      args <- x$conn_args
      args$schema <- NULL
      conn <- do.call(DBI::dbConnect, args)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
    }
  } else if (inherits(x, "TableModel")) {
    conn <- x$engine$conn
    if (is.null(conn) || !DBI::dbIsValid(conn)) {
      args <- x$engine$conn_args
      args$schema <- NULL
      conn <- do.call(DBI::dbConnect, args)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
    }
  }

  sql <- paste0("CREATE SCHEMA IF NOT EXISTS ", DBI::dbQuoteIdentifier(conn, schema))
  DBI::dbExecute(conn, sql)
  invisible(NULL)
}


