Record$set("public", "update", function() {
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
    stop("Cannot update without all primary key fields: ",
         paste(missing_keys, collapse = ", "))
  }

  # Separate key and non-key fields
  non_key_fields <- setdiff(names(self$data), key_fields)
  if (length(non_key_fields) == 0) {
    stop("No non-key fields to update.")
  }

  # Build SET clause
  set_clause <- paste0(
    non_key_fields, " = ",
    sapply(self$data[non_key_fields], DBI::dbQuoteLiteral, conn = con),
    collapse = ", "
  )

  # Build WHERE clause
  where_clause <- paste0(
    key_fields, " = ",
    sapply(self$data[key_fields], DBI::dbQuoteLiteral, conn = con),
    collapse = " AND "
  )

  sql <- sprintf(
    "UPDATE %s SET %s WHERE %s",
    DBI::dbQuoteIdentifier(con, self$model$tablename),
    set_clause,
    where_clause
  )

  DBI::dbExecute(con, sql)
})
