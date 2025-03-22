#' Delete rows matching a filter expression
#'
#' This method deletes rows from the associated table based on the provided filter expressions.
#' It uses dplyr-style filter expressions to specify the rows to be deleted.
#'
#' @name BaseModel$delete_where
#' @param ... Unquoted dplyr-style filter expressions.
#'   These expressions should be used to filter the rows to be deleted.
#'   For example, `age > 18` will delete all rows where the `age` column is greater than 18.
#' @return The number of rows deleted.
#' @examples
#' \dontrun{
#' # Delete all rows where the 'age' column is greater than 18
#' my_model$delete_where(age > 18)
#'
#' # Delete all rows where the 'name' column is 'Alice' and 'age' is less than 30
#' my_model$delete_where(name == "Alice", age < 30)
#' }
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