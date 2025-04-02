flush <- function(x, ...) {
  UseMethod("flush")
}

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

flush.sqlite <- function(x, table, data, con, commit = TRUE, ...) {
  tbl_expr <- dbplyr::ident_q(table)
  fields <- names(data)

  values_sql <- paste0("(", paste(DBI::dbQuoteLiteral(con, unname(data)), collapse = ", "), ")")
  field_sql <- paste(DBI::dbQuoteIdentifier(con, fields), collapse = ", ")

  sql <- paste0(
    "INSERT INTO ", tbl_expr, " (", field_sql, ") VALUES ", values_sql
  )

  DBI::dbExecute(con, sql)
  id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid();")[[1]]

  # Optionally commit or return the id
  if (commit) {
    DBI::dbCommit(con)
  }

  return(id)
}

flush.default <- local({
  warned <- FALSE

  function(x, table, data, con, commit = TRUE, ...) {
    if (!warned) {
      warning("This dialect is not explicitly supported. Proceeding with insert without returning results.")
      warned <<- TRUE
    }

    tbl_expr <- dbplyr::ident_q(table)
    fields <- names(data)

    values_sql <- paste0("(", paste(DBI::dbQuoteLiteral(con, unname(data)), collapse = ", "), ")")
    field_sql <- paste(DBI::dbQuoteIdentifier(con, fields), collapse = ", ")

    sql <- paste0(
      "INSERT INTO ", tbl_expr, " (", field_sql, ") VALUES ", values_sql
    )

    DBI::dbExecute(con, sql)

    if (commit) {
      DBI::dbCommit(con)
    }

    invisible(NULL)
  }
})
