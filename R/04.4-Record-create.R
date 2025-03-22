Record$set("public", "create", function() {
  con <- self$model$get_connection()

  # Check required fields
  required_fields <- names(self$model$fields)[
    vapply(self$model$fields, function(x) !x$nullable, logical(1))
  ]
  missing_fields <- setdiff(required_fields, names(self$data))
  if (length(missing_fields) > 0) {
    stop("Missing required fields: ", paste(missing_fields, collapse = ", "))
  }

  # Insert the row
  DBI::dbAppendTable(
    conn = con,
    name = self$model$tablename,
    value = as.data.frame(self$data, stringsAsFactors = FALSE)
  )
})
