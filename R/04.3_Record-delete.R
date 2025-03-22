Record$set("public", "delete", function() {
  con <- self$model$get_connection()

  # Identify key fields
  key_fields <- names(self$model$fields)[
    vapply(self$model$fields, function(x) isTRUE(x$key), logical(1))
  ]

  if (length(key_fields) == 0) {
    stop("No primary key fields defined in model.")
  }

  # Ensure all key fields are in self$data
  missing_keys <- setdiff(key_fields, names(self$data))
  if (length(missing_keys) > 0) {
    stop("Cannot delete without all primary key fields: ",
         paste(missing_keys, collapse = ", "))
  }

  # Build WHERE clause
  where_clause <- paste0(
    key_fields, " = ",
    sapply(self$data[key_fields], DBI::dbQuoteLiteral, conn = con),
    collapse = " AND "
  )

  sql <- sprintf(
    "DELETE FROM %s WHERE %s",
    DBI::dbQuoteIdentifier(con, self$model$tablename),
    where_clause
  )

  DBI::dbExecute(con, sql)
})
