flush.postgres <- function(x, table, data, con, commit = TRUE, ...) {
  # Build the insert SQL
  tbl_expr <- dbplyr::ident_q(table)
  fields <- names(data)

  values_sql <- paste0("(", paste(DBI::dbQuoteLiteral(con, unname(data)), collapse = ", "), ")")
  field_sql <- paste(DBI::dbQuoteIdentifier(con, fields), collapse = ", ")

  sql <- paste0(
    "INSERT INTO ", tbl_expr, " (", field_sql, ") VALUES ", values_sql,
    " RETURNING *"
  )

  result <- DBI::dbGetQuery(con, sql)

  # Optionally commit or return results
  if (commit) {
    DBI::dbCommit(con)
  }

  return(result)
}