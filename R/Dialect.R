#' include Dialect-mysql.R
#' include Dialect-postgres.R
#' include Dialect-sqlite.R
NULL

#' Set the schema for a table based on the database dialect
#'
#' Allows callers to specify a schema separately from the table name. The
#' returned value should be the fully qualified identifier used by the
#' underlying database. Dialects that do not support schemas may simply
#' combine the schema and table name into a single identifier.
#'
#' @param table The table name
#' @param schema The schema name
#' @param dialect A character string identifying the database dialect
#'
#' @return A character string representing the schema-qualified table name
#' @export
set_schema <- function(table, schema, dialect) UseMethod("set_schema", dialect)

#' @export
set_schema.default <- function(table, schema, dialect) {
  if (is.null(schema) || schema == "") {
    table
  } else {
    paste0(schema, ".", table)
  }
}

#' Render a column field to SQL
#'
#' @param field A Column object
#' @param field_name the name of the column
#' @param conn a DBI connection object
#' @param ... Ignored
#' @return A character SQL fragment
#' @export
render_field <- function(field, conn, ...) {
    UseMethod("render_field", field)
}

add_part <- function(parts, field, true, false = NULL) {
    if(is.null(field))
        return(parts)
    
    parts <- c(parts, if (field) true else false)
    parts
}

add_extras <- function(parts, fields) {
    if (!is.null(fields) && length(fields) > 0) 
        parts <- c(parts, unlist(fields))
    parts
}

#' Write the sql for creating a sql column
#'
#' @inheritParams render_field
#' @export
render_field.default <- function(field, conn, ...) {
    parts = c(field$type)
    
    parts = parts |>
        add_part(field$nullable, NULL, "NOT NULL") |>
        add_part(field$unique, "UNIQUE") |>
        add_part(field$primary_key, "PRIMARY KEY") |>
        add_extras(field$extras)
        
    # if a user wants to use sql() to enter a sql function like CURRENT_TIMESTAMP
    if (!is.null(field$default)) {
        if (inherits(field$default, "sql")) {
            parts <- c(parts, "DEFAULT", field$default)
        } else if (!is.function(field$default)) {
            parts <- c(parts, "DEFAULT", DBI::dbQuoteLiteral(DBI::ANSI(), field$default))
        }
    }
        
    parts_string = paste(parts, collapse = ' ')
    paste(DBI::dbQuoteIdentifier(conn, field$name), parts_string)
}

#' Write teh sql for a foreign key column
#' @inheritParams render_field
#' @export
render_constraint <- function(field, ...) {
    UseMethod("render_constraint", field)
}

#' @inheritParams render_field
#' @export
render_constraint.default <- function(field, conn, ...) {
    fk_parts = c()
    if (inherits(field, 'ForeignKey')) {
        reference_parts = strsplit(field$references, '\\.')[[1]]
        stopifnot(
            "Invalid foreign key reference format. Expected 'table.column'" =
            length(reference_parts) == 2
        )
        fk_parts = c(fk_parts, paste0(
            "FOREIGN KEY (", DBI::dbQuoteIdentifier(conn, field$name), ") REFERENCES ",
            DBI::dbQuoteIdentifier(conn, reference_parts[1]), " (",
            DBI::dbQuoteIdentifier(conn, reference_parts[2]), ")"
        ))
        if (!is.null(field$on_delete))
        fk_parts = c(fk_parts, paste("ON DELETE", toupper(field$on_delete)))
        if (!is.null(field$on_update))
        fk_parts = c(fk_parts, paste('ON UPDATE', toupper(field$on_update)))
    } else {
        return(NULL)
    }
    
    fk_string = paste(fk_parts, collapse  = ' ')
    fk_string
}


flush <- function(x, table, data, con, commit = TRUE, ...) {
  # Get the dialect
  dialect <- if (inherits(x, "Engine")) {
    x$dialect
  } else if (inherits(x, "TableModel")) {
    x$engine$dialect
  } else if (inherits(x, "Record")) {
    x$model$engine$dialect
  } else {
    "default"
  }
  
  # Find the appropriate method
  method_name <- paste0("flush.", dialect)
  method <- get0(method_name, mode = "function", ifnotfound = flush.default)
  
  # Call the method
  method(x, table, data, con, commit, ...)
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
