flush.mysql <- function(x, table, data, con, commit = TRUE, ...) {
  tbl_expr <- dbplyr::ident_q(table)
  fields <- names(data)

  values_sql <- paste0("(", paste(DBI::dbQuoteLiteral(con, unname(data)), collapse = ", "), ")")
  field_sql <- paste(DBI::dbQuoteIdentifier(con, fields), collapse = ", ")

  sql <- paste0(
    "INSERT INTO ", tbl_expr, " (", field_sql, ") VALUES ", values_sql
  )

  DBI::dbExecute(con, sql)
  id <- DBI::dbGetQuery(con, "SELECT LAST_INSERT_ID();")[[1]]

  # Optionally commit or return the id
  if (commit) {
    DBI::dbCommit(con)
  }

  return(id)
}