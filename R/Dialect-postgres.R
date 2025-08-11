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

#' @export
set_schema.postgres <- function(table, schema, dialect) {
  if (is.null(schema) || schema == "") {
    table
  } else {
    paste0(schema, ".", table)
  }
}


