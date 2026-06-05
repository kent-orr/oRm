# Internal SQL-construction helpers shared by Record (row-level) and
# TableModel (set-level) write operations. Keeping these in one place avoids
# duplicating value-quoting / JSON-serialization logic across classes.

#' Build a SQL SET clause from a named list of values
#'
#' Produces a `col = <literal>, ...` fragment, serializing JSON/JSONB columns
#' for PostgreSQL the same way `Record$update()` historically did. Column
#' identifiers are left bare to match the surrounding query-building style.
#'
#' @param con A DBI connection used for literal quoting.
#' @param fields The model's field definitions (`model$fields`), used to detect
#'   JSON/JSONB columns.
#' @param values A named list of column -> value to assign.
#' @param dialect Character dialect name (JSON handling applies to "postgres").
#' @return A single-string SET clause.
#' @noRd
build_set_clause <- function(con, fields, values, dialect = NULL) {
    json_fields <- character(0)
    if (identical(dialect, "postgres")) {
        json_fields <- names(fields)[vapply(fields, function(f) {
            toupper(f$type) %in% c("JSON", "JSONB")
        }, logical(1))]
    }

    set_parts <- vapply(names(values), function(field_name) {
        val <- values[[field_name]]

        if (field_name %in% json_fields) {
            if (is.character(val) && length(val) == 1 && jsonlite::validate(val)) {
                formatted_val <- val
            } else {
                formatted_val <- jsonlite::toJSON(val, auto_unbox = TRUE)
            }
            quoted_val <- DBI::dbQuoteLiteral(con, as.character(formatted_val))
        } else {
            quoted_val <- DBI::dbQuoteLiteral(con, val)
        }

        paste0(field_name, " = ", quoted_val)
    }, character(1))

    paste(set_parts, collapse = ", ")
}

#' Primary-key field names for a model, erroring if none are defined
#'
#' Shared by row-level (`Record`) and set-level (`TableModel`) write/refresh
#' operations, all of which key on the primary key.
#'
#' @param model A TableModel.
#' @return Character vector of primary-key column names.
#' @noRd
pk_fields <- function(model) {
    fields <- model$fields
    keys <- names(fields)[vapply(fields, function(x) isTRUE(x$primary_key), logical(1))]
    if (length(keys) == 0) {
        stop(
            "No primary key fields defined in model; update/delete/refresh requires a primary key.",
            call. = FALSE
        )
    }
    keys
}

#' Build a SQL WHERE clause that matches a set of primary-key rows
#'
#' Single-column keys produce `key IN (...)`; composite keys produce an
#' OR-of-ANDs, one parenthesized group per row.
#'
#' @param con A DBI connection used for literal quoting.
#' @param key_fields Character vector of primary-key column names.
#' @param keys A data.frame whose columns include `key_fields`, one row per
#'   record to match.
#' @return A single-string WHERE clause (without the leading "WHERE").
#' @noRd
build_key_where <- function(con, key_fields, keys) {
    if (length(key_fields) == 1) {
        k <- key_fields[[1]]
        vals <- vapply(keys[[k]], function(v) DBI::dbQuoteLiteral(con, v), character(1))
        return(sprintf("%s IN (%s)", k, paste(vals, collapse = ", ")))
    }

    rows <- vapply(seq_len(nrow(keys)), function(i) {
        conds <- vapply(key_fields, function(k) {
            paste0(k, " = ", DBI::dbQuoteLiteral(con, keys[[k]][[i]]))
        }, character(1))
        paste0("(", paste(conds, collapse = " AND "), ")")
    }, character(1))

    paste(rows, collapse = " OR ")
}
