#' Delete rows matching a filter expression
#' @param ... Unquoted dplyr-style filter expressions
#' @export
BaseModel$set("public", "delete_where", function(...) {
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
})
