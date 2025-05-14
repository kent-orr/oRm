#' include Dialect-mysql.R
#' include Dialect-postgres.R
#' include Dialect-sqlite.R
NULL

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

#' Write the sql for creating a sql column
#'
#' @inheritParams render_field
#' @export
render_field.default <- function(field, conn, ...) {
  parts = c(field$type)

  if (!is.null(field$nullable))
    parts <- c(parts, if (field$nullable) "NULL" else "NOT NULL")

  if (!is.null(field$default) && !is.function(field$default))
    parts <- c(parts, "DEFAULT", DBI::dbQuoteLiteral(DBI::ANSI(), field$default))

  if (!is.null(field$unique) && field$unique)
    parts <- c(parts, "UNIQUE")

  if (!is.null(field$primary_key) && field$primary_key)
    parts <- c(parts, "PRIMARY KEY")

  if (!is.null(field$extras) && length(field$extras) > 0)
    parts <- c(parts, unlist(field$extras))

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
