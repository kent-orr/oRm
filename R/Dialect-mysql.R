#' @describeIn flush MySQL insert returning the last generated ID.
flush.mysql <- function(x, table, data, con, commit = TRUE, ...) {
    tbl_expr <- dbplyr::ident_q(table)
    fields <- names(data)

    values_sql <- paste0("(", paste(DBI::dbQuoteLiteral(con, unname(data)), collapse = ", "), ")")
    field_sql <- paste(DBI::dbQuoteIdentifier(con, fields), collapse = ", ")

    sql <- paste0(
        "INSERT INTO ", tbl_expr, " (", field_sql, ") VALUES ", values_sql
    )

    DBI::dbExecute(con, sql)
    id <- DBI::dbGetQuery(con, "SELECT LAST_INSERT_ID();")[[1]]

    # Optionally commit or return the id
    if (commit) {
        DBI::dbCommit(con)
    }

    return(id)
}


#' @describeIn qualify MySQL-specific table qualification with optional schema.
qualify.mysql <- function(x, tablename, schema) {
    if (!grepl("\\.", tablename) && !is.null(schema)) {
        paste(schema, tablename, sep = ".")
    } else {
        tablename
    }
}


#' @describeIn set_schema Change the active schema using MySQL's USE statement.
set_schema.mysql <- function(x, schema) {
    conn <- if (inherits(x, "Engine")) x$get_connection() else x$engine$get_connection()
    sql <- paste0("USE ", DBI::dbQuoteIdentifier(conn, schema))
    DBI::dbExecute(conn, sql)
    invisible(NULL)
}

#' @describeIn check_schema_exists Check if a schema exists for MySQL.
check_schema_exists.mysql <- function(x, schema) {
    if (is.null(schema)) return(TRUE)

    conn <- NULL
    if (inherits(x, "Engine")) {
        conn <- x$conn
        if (is.null(conn) || !DBI::dbIsValid(conn)) {
            args <- x$conn_args
            args$dbname <- NULL
            conn <- do.call(DBI::dbConnect, args)
            on.exit(DBI::dbDisconnect(conn), add = TRUE)
        }
    } else if (inherits(x, "TableModel")) {
        conn <- x$engine$conn
        if (is.null(conn) || !DBI::dbIsValid(conn)) {
            args <- x$engine$conn_args
            args$dbname <- NULL
            conn <- do.call(DBI::dbConnect, args)
            on.exit(DBI::dbDisconnect(conn), add = TRUE)
        }
    }

    sql <- paste0(
        "SELECT SCHEMA_NAME FROM INFORMATION_SCHEMA.SCHEMATA WHERE SCHEMA_NAME = ",
        DBI::dbQuoteLiteral(conn, schema)
    )
    exists <- FALSE
    try({
        res <- DBI::dbGetQuery(conn, sql)
        exists <- NROW(res) > 0
    }, silent = TRUE)
    exists
}
