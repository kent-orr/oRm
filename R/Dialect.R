#' include Dialect-mysql.R
#' include Dialect-postgres.R
#' include Dialect-sqlite.R
NULL

generate_sql <- function(x, ...) {
  UseMethod("generate_sql_fields")
}

generate_sql.default <- function(x, ...) {
  out <- list()
  constraints <- list()
  con <- self$get_connection()
  
  for (name in names(self$fields)) {
    col <- self$fields[[name]]
    parts <- c(DBI::dbQuoteIdentifier(con, name), col$type)
    
    if (!is.null(col$nullable)) {
      if (!col$nullable) {
        parts <- c(parts, "NOT NULL")
      } else {
        parts <- c(parts, "NULL")
      }
    }
    
    if (!is.null(col$default) && !is.function(col$default)) {
      parts <- c(parts, "DEFAULT", DBI::dbQuoteLiteral(con, col$default))
    }
    
    if (!is.null(col$unique) && col$unique) {
      parts <- c(parts, "UNIQUE")
    }
    
    if (!is.null(col$primary_key) && col$primary_key) {
      parts <- c(parts, "PRIMARY KEY")
    }
    
    if (!is.null(col$extras) && length(col$extras) > 0) {
      parts <- c(parts, unlist(col$extras))
    }
    
    field_sql <- paste(parts, collapse = " ")
    out[[name]] <- field_sql
    
    # If it's a ForeignKey column, add explicit FOREIGN KEY clause
    if (inherits(col, "ForeignKey")) {
      ref_parts <- strsplit(col$references, "\\.")[[1]]
      if (length(ref_parts) != 2) {
        stop("Invalid foreign key reference format. Expected 'table.column', got: ", col$references)
      }
      fk_clause <- paste0(
        "FOREIGN KEY (", DBI::dbQuoteIdentifier(con, name), ") REFERENCES ",
        DBI::dbQuoteIdentifier(con, ref_parts[1]), " (",
        DBI::dbQuoteIdentifier(con, ref_parts[2]), ")"
      )
      
      if (!is.null(col$on_delete)) {
        fk_clause <- paste(fk_clause, "ON DELETE", toupper(col$on_delete))
      }
      
      if (!is.null(col$on_update)) {
        fk_clause <- paste(fk_clause, "ON UPDATE", toupper(col$on_update))
      }
      
      constraints[[length(constraints) + 1]] <- fk_clause
    }
  }
  
  # Append constraints as additional definitions
  if (length(constraints)) {
    for (i in seq_along(constraints)) {
      out[[paste0("__constraint", i)]] <- constraints[[i]]
    }
  }
  
  out
}


flush <- function(x, ...) {
  UseMethod("flush")
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
