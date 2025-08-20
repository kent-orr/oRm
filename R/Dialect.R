#' @include Dialect-mysql.R Dialect-postgres.R Dialect-sqlite.R
NULL


#' Get the dialect from an oRm object
#'
#' This internal function extracts the database dialect from various oRm objects
#' by traversing their object hierarchy to find the associated Engine.
#'
#' @param x An oRm object (Engine, TableModel, or Record)
#'
#' @return Character string representing the dialect name, or "default" if no dialect is found
#' @keywords internal
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


#' Dispatch method calls based on dialect
#'
#' This internal function provides a mechanism for method dispatch based on the
#' dialect of the database engine. It looks for dialect-specific implementations
#' first, then falls back to default implementations.
#'
#' @param x An object that has an associated dialect (Engine, TableModel, or Record)
#' @param method Character string naming the method to dispatch
#' @param ... Additional arguments passed to the dispatched method
#'
#' @return The result of calling the appropriate dialect-specific or default method
#' @keywords internal
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


#' Qualify table name with schema
#'
#' This internal function adds schema qualification to table names when needed,
#' using dialect-specific logic for different database systems.
#'
#' @param x An oRm object (Engine, TableModel, or Record) used for dialect dispatch
#' @param tablename Character string of the table name to qualify
#' @param schema Character string of the schema name, or NULL
#'
#' @return Character string of the qualified table name
#' @keywords internal
qualify <- function(x, tablename, schema) {
    dispatch_method(x, "qualify", tablename, schema)
}

#' @rdname qualify
#' @keywords internal
qualify.default <- function(x, tablename, schema) {
    if (!grepl("\\.", tablename) && !is.null(schema)) {
        paste(schema, tablename, sep = ".")
    } else {
        tablename
    }
}


# Schema -----------------------------------------------------------------


#' Set schema for database operations
#'
#' This internal function sets the schema context for database operations,
#' using dialect-specific logic for different database systems.
#'
#' @param x An oRm object (Engine, TableModel, or Record) used for dialect dispatch
#' @param schema Character string of the schema name to set
#'
#' @return Invisible NULL (called for side effects)
#' @keywords internal
set_schema <- function(x, schema) {
    dispatch_method(x, "set_schema", schema)
}

#' @rdname set_schema
#' @keywords internal
set_schema.default <- function(x, schema) {
    invisible(NULL)
}

#' Check whether a schema exists for the current dialect
#'
#' Dialects that do not implement schemas should return TRUE.
#'
#' @param x Engine or TableModel instance used for dispatch.
#' @param schema Character. Name of the schema to check.
#' @keywords internal
check_schema_exists <- function(x, schema) {
    dispatch_method(x, "check_schema_exists", schema)
}

#' @rdname check_schema_exists
#' @keywords internal
check_schema_exists.default <- function(x, schema) {
    stop("check_schema_exists() is not implemented for this database dialect.", call. = FALSE)
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

#' Default method that performs no action
#' 
#' @rdname ensure_schema_exists
#' @keywords internal
ensure_schema_exists.default <- function(x, schema) {
    invisible(NULL)
}

#' Create a schema for the current dialect
#'
#' By default, this is a no-op. Implement dialect-specific ones as needed.
#'
#' @param x An Engine or TableModel instance used for dispatch.
#' @param schema Character. Name of the schema to create.
#' @keywords internal
create_schema <- function(x, schema) {
    dispatch_method(x, "create_schema", schema)
}

#' @rdname create_schema
#' @keywords internal
create_schema.default <- function(x, schema) {
    stop("create_schema() is not implemented for this database dialect.", call. = FALSE)
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

#' Add conditional SQL parts to column definition
#'
#' This internal helper function conditionally adds SQL fragments to a column
#' definition based on field properties.
#'
#' @param parts Character vector of existing SQL parts
#' @param field Logical value or NULL indicating whether to add the SQL fragment
#' @param true Character string to add when field is TRUE
#' @param false Character string to add when field is FALSE (optional)
#'
#' @return Character vector with potentially added SQL parts
#' @keywords internal
add_part <- function(parts, field, true, false = NULL) {
    if(is.null(field))
    return(parts)
    
    parts <- c(parts, if (field) true else false)
    parts
}

#' Add extra SQL parts to column definition
#'
#' This internal helper function adds additional SQL fragments from a list
#' of extras to the column definition.
#'
#' @param parts Character vector of existing SQL parts
#' @param fields List or vector of additional SQL fragments to add
#'
#' @return Character vector with added SQL parts
#' @keywords internal
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




#' Flush data to database table
#'
#' This internal function inserts data into a database table using dialect-specific
#' logic for different database systems. It handles the actual database write operation
#' for Record objects.
#'
#' @param x An oRm object (Engine, TableModel, or Record) used for dialect dispatch
#' @param table Character string of the table name
#' @param data Named list or vector of data to insert
#' @param con DBI connection object
#' @param commit Logical indicating whether to commit the transaction
#' @param ... Additional arguments passed to dialect-specific methods
#'
#' @return Invisible NULL or dialect-specific return value
#' @keywords internal
flush <- function(x, table, data, con, commit = TRUE, ...) {
    dispatch_method(x, "flush", table, data, con, commit,...)
}

#' @rdname flush
#' @keywords internal
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

