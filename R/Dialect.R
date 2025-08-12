#' include Dialect-mysql.R
#' include Dialect-postgres.R
#' include Dialect-sqlite.R
NULL


get_dialect <- function(x) {
    if (inherits(x, "Engine")) {
        x$dialect
    } else if (inherits(x, "TableModel")) {
        x$engine$dialect
    } else if (inherits(x, "Record")) {
        x$model$engine$dialect
    } else {
        "default"
    }
}

dispatch_method <- function(x, method, ...) {
    dialect <- get_dialect(x)

    method_name <- paste(method, dialect, sep = '.')
    method_fn <- get0(method_name, mode = "function")
    if (is.null(method_fn)) {
        method_fn <- get0(paste0(method, '.default'), mode = "function")
    }
    method_fn(x, ...)
}

# Qualify Schema ---------------------------------------------------------

qualify <- function(x, tablename, schema) {
    dispatch_method(x, "qualify", tablename, schema)
}

qualify.default <- function(x, tablename, schema) {
    if (!grepl("\\.", tablename) && !is.null(schema)) {
        paste(schema, tablename, sep = ".")
    } else {
        tablename
    }
}


# Schema -----------------------------------------------------------------

set_schema <- function(x, schema) {
    dispatch_method(x, "set_schema", schema)
}

set_schema.default <- function(x, schema) {
    invisible(NULL)
}

#' Ensure that a schema exists for the current dialect
#'
#' This utility creates the schema if the connected database supports it.
#' Dialects that do not implement schemas should provide a no-op.
#'
#' @param x Engine or TableModel instance used for dispatch.
#' @param schema Character. Name of the schema to create.
#' @keywords internal
ensure_schema_exists <- function(x, schema) {
    dispatch_method(x, "ensure_schema_exists", schema)
}

ensure_schema_exists.default <- function(x, schema) {
    invisible(NULL)
}


# Render -----------------------------------------------------------------



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
    fk_parts <- c()
    if (inherits(field, 'ForeignKey')) {
        if (is.null(field$ref_table) || is.null(field$ref_column)) {
            stop(sprintf("ForeignKey '%s' must define 'ref_table' and 'ref_column'.", field$name))
        }

        validate_identifier(field$ref_table, "ref_table")
        validate_identifier(field$ref_column, "ref_column")

        fk_parts <- c(fk_parts, paste0(
            "FOREIGN KEY (", DBI::dbQuoteIdentifier(conn, field$name), ") REFERENCES ",
            DBI::dbQuoteIdentifier(conn, field$ref_table), " (",
            DBI::dbQuoteIdentifier(conn, field$ref_column), ")"
        ))
        if (!is.null(field$on_delete))
            fk_parts <- c(fk_parts, paste("ON DELETE", toupper(field$on_delete)))
        if (!is.null(field$on_update))
            fk_parts <- c(fk_parts, paste('ON UPDATE', toupper(field$on_update)))
    } else {
        return(NULL)
    }

    fk_string <- paste(fk_parts, collapse = ' ')
    fk_string
}


# Flush ------------------------------------------------------------------



flush <- function(x, table, data, con, commit = TRUE, ...) {
    dispatch_method(x, "flush", table, data, con, commit,...)
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
