# Microsoft SQL Server dialect.
#
# Selected automatically when an odbc connection names SQL Server in its
# `Driver`/`.connection_string` argument, or explicitly via
# `Engine$new(..., .dialect = "mssql")`. SQL Server 2016 or later is assumed
# (for `DROP TABLE IF EXISTS`).

#' @describeIn sql_create_table T-SQL has no `CREATE TABLE IF NOT EXISTS`, so
#'   the guard becomes an `IF OBJECT_ID(...) IS NULL` block.
sql_create_table.mssql <- function(x, conn, tablename, quoted_name, body, if_not_exists = TRUE, ...) {
    create <- paste0("CREATE TABLE ", quoted_name, " (\n    ", body, "\n);\n")
    if (!if_not_exists) {
        return(create)
    }
    paste0(
        "IF OBJECT_ID(N", DBI::dbQuoteLiteral(conn, tablename), ", N'U') IS NULL\n",
        "BEGIN\n", create, "END;\n"
    )
}

#' @rdname flush
#' @usage \method{flush}{mssql}(x, table, data, con, commit = TRUE, ...)
#' @description Insert a row and return the inserted record using SQL Server's
#'   `OUTPUT INSERTED.*` clause. IDENTITY columns are dropped from the insert
#'   list, since assigning them requires `SET IDENTITY_INSERT`.
flush.mssql <- function(x, table, data, con, commit = TRUE, ...) {
    data <- data[!vapply(data, is.null, logical(1))]

    dots <- list(...)
    field_defs <- dots$fields

    # Server-generated columns cannot appear in the insert list.
    if (!is.null(field_defs)) {
        identity_fields <- names(field_defs)[
            vapply(field_defs, function(f) is_auto_generated_type(f$type), logical(1))
        ]
        data <- data[setdiff(names(data), identity_fields)]
    }

    tbl_expr <- dbplyr::ident_q(table)
    fields <- names(data)

    # Date/time values have to reach SQL Server as literals it parses, so
    # normalize once and reuse for both the OUTPUT insert and the fallback.
    data <- lapply(data, function(value) {
        if (inherits(value, "Date")) {
            as.character(value)
        } else if (inherits(value, "POSIXt")) {
            format(value, "%Y-%m-%d %H:%M:%S")
        } else {
            value
        }
    })

    if (length(fields) == 0) {
        sql <- paste0("INSERT INTO ", tbl_expr, " OUTPUT INSERTED.* DEFAULT VALUES")
    } else {
        values_sql <- paste0(
            "(",
            paste(
                DBI::dbQuoteLiteral(con, unlist(data, use.names = FALSE)),
                collapse = ", "
            ),
            ")"
        )
        field_sql <- paste(DBI::dbQuoteIdentifier(con, fields), collapse = ", ")

        sql <- paste0(
            "INSERT INTO ", tbl_expr, " (", field_sql, ") ",
            "OUTPUT INSERTED.* VALUES ", values_sql
        )
    }

    tryCatch(
        DBI::dbGetQuery(con, sql),
        error = function(e) {
            # SQL Server rejects OUTPUT without INTO on tables carrying triggers
            # (error 334). Fall back to a plain insert plus a keyed re-read,
            # which is only possible when the caller supplied the key values.
            if (!grepl("334|OUTPUT clause|trigger", conditionMessage(e), ignore.case = TRUE)) {
                stop(e)
            }
            mssql_flush_fallback(table, data, con, field_defs, conditionMessage(e))
        }
    )
}

#' Re-read an inserted row when OUTPUT INSERTED is unavailable
#'
#' Used by [flush.mssql] when the target table has triggers. Requires the
#' primary key to be supplied by the caller, because there is no reliable way to
#' recover a server-generated key once `OUTPUT` is off the table.
#' @keywords internal
#' @noRd
mssql_flush_fallback <- function(table, data, con, field_defs, original_message) {
    keys <- character(0)
    if (!is.null(field_defs)) {
        keys <- names(field_defs)[vapply(field_defs, function(f) isTRUE(f$primary_key), logical(1))]
    }

    if (length(keys) == 0 || !all(keys %in% names(data))) {
        stop(
            "flush failed on a table where OUTPUT INSERTED is unavailable (SQL Server ",
            "rejects it on tables with triggers). Supply the primary key values ",
            "explicitly so the inserted row can be re-read, or drop the trigger. ",
            "Original error: ", original_message,
            call. = FALSE
        )
    }

    tbl_expr <- dbplyr::ident_q(table)
    field_sql <- paste(DBI::dbQuoteIdentifier(con, names(data)), collapse = ", ")
    values_sql <- paste0(
        "(",
        paste(DBI::dbQuoteLiteral(con, unlist(data, use.names = FALSE)), collapse = ", "),
        ")"
    )
    DBI::dbExecute(con, paste0(
        "INSERT INTO ", tbl_expr, " (", field_sql, ") VALUES ", values_sql
    ))

    where <- paste(
        vapply(keys, function(k) {
            paste0(DBI::dbQuoteIdentifier(con, k), " = ", DBI::dbQuoteLiteral(con, data[[k]]))
        }, character(1)),
        collapse = " AND "
    )
    DBI::dbGetQuery(con, paste0("SELECT * FROM ", tbl_expr, " WHERE ", where))
}

#' @describeIn set_schema SQL Server has no session-level schema switch (the
#'   default schema is a property of the login), so schemas are applied purely
#'   by name qualification, which `qualify.default` already handles.
set_schema.mssql <- function(x, .schema) {
    invisible(NULL)
}

#' @describeIn check_schema_exists Check if a schema exists for SQL Server.
check_schema_exists.mssql <- function(x, .schema) {
    if (is.null(.schema)) return(TRUE)

    conn <- NULL
    if (inherits(x, "Engine")) {
        conn <- x$get_connection()
    } else if (inherits(x, "TableModel")) {
        conn <- x$engine$get_connection()
    }

    if (is.null(conn) || !DBI::dbIsValid(conn)) {
        return(FALSE)
    }

    sql <- paste0(
        "SELECT 1 FROM sys.schemas WHERE name = ", DBI::dbQuoteLiteral(conn, .schema)
    )
    exists <- FALSE
    try({
        res <- DBI::dbGetQuery(conn, sql)
        exists <- NROW(res) > 0
    }, silent = TRUE)
    exists
}

#' @describeIn create_schema Create the schema for SQL Server. `CREATE SCHEMA`
#'   must be the only statement in its batch, so it is run through `EXEC`.
create_schema.mssql <- function(x, .schema) {
    if (is.null(.schema)) stop("Must supply a schema name.", call. = FALSE)

    engine <- if (inherits(x, "Engine")) x else x$engine

    if (is.null(engine$conn) || !DBI::dbIsValid(engine$conn)) {
        conn <- do.call(DBI::dbConnect, engine$conn_args)
        on.exit(DBI::dbDisconnect(conn), add = TRUE)
    } else {
        conn <- engine$conn
    }

    # CREATE SCHEMA must be the only statement in its batch, so it runs through
    # EXEC. EXEC's argument only accepts string literals and variables — not a
    # concatenation involving QUOTENAME() — so the statement is built here and
    # passed as a single literal.
    create_stmt <- paste0("CREATE SCHEMA ", DBI::dbQuoteIdentifier(conn, .schema))
    sql <- paste0(
        "IF NOT EXISTS (SELECT 1 FROM sys.schemas WHERE name = ",
        DBI::dbQuoteLiteral(conn, .schema), ") ",
        "EXEC(", DBI::dbQuoteLiteral(conn, create_stmt), ")"
    )
    DBI::dbExecute(conn, sql)
    invisible(TRUE)
}

#' @describeIn apply_read_only SQL Server has no session-level read-only mode
#'   (`ApplicationIntent` only routes to availability-group replicas), so
#'   read-only engines fall back to oRm's application-level statement guard.
apply_read_only.mssql <- function(x, con) {
    warning(
        "SQL Server provides no session-level read-only mode; this engine is ",
        "read-only by application-level guards only.",
        call. = FALSE
    )
    invisible(NULL)
}


# Reflection -------------------------------------------------------------

#' Render a SQL Server type from its `sys.columns` metadata
#'
#' Reconstructs the declared type, including length/precision/scale and any
#' IDENTITY specification, so a reflected model can recreate the column.
#'
#' @param type_name Base type name from `sys.types`.
#' @param max_length Byte length from `sys.columns`; -1 means MAX.
#' @param precision,scale Numeric precision and scale from `sys.columns`.
#' @param is_identity Logical, whether the column is an IDENTITY column.
#' @param seed,increment IDENTITY seed and increment from `sys.identity_columns`.
#' @return A character type declaration.
#' @keywords internal
mssql_format_type <- function(type_name, max_length, precision, scale,
                              is_identity = FALSE, seed = NULL, increment = NULL) {
    type_name <- as.character(type_name)
    lower <- tolower(type_name)

    out <- if (lower %in% c("varchar", "char", "binary", "varbinary")) {
        if (isTRUE(max_length == -1)) {
            paste0(type_name, "(MAX)")
        } else {
            paste0(type_name, "(", max_length, ")")
        }
    } else if (lower %in% c("nvarchar", "nchar")) {
        # sys.columns reports bytes; Unicode types store two bytes per character.
        if (isTRUE(max_length == -1)) {
            paste0(type_name, "(MAX)")
        } else {
            paste0(type_name, "(", max_length %/% 2L, ")")
        }
    } else if (lower %in% c("decimal", "numeric")) {
        paste0(type_name, "(", precision, ",", scale, ")")
    } else if (lower %in% c("datetime2", "time", "datetimeoffset")) {
        paste0(type_name, "(", scale, ")")
    } else {
        type_name
    }

    if (isTRUE(is_identity)) {
        seed <- if (is.null(seed) || is.na(seed)) 1 else seed
        increment <- if (is.null(increment) || is.na(increment)) 1 else increment
        out <- paste0(
            out, " IDENTITY(", format(seed, scientific = FALSE), ",",
            format(increment, scientific = FALSE), ")"
        )
    }

    out
}

#' Map a SQL Server referential-action description to a SQL action
#'
#' `sys.foreign_keys` reports actions as `NO_ACTION`, `CASCADE`, `SET_NULL` or
#' `SET_DEFAULT`. `NO_ACTION` is the implicit default and is returned as NULL so
#' it is not rendered.
#' @keywords internal
mssql_fk_action <- function(desc) {
    if (is.null(desc) || is.na(desc)) return(NULL)
    desc <- toupper(as.character(desc))
    if (desc == "NO_ACTION") return(NULL)
    gsub("_", " ", desc)
}

#' @describeIn reflect_columns SQL Server reflection via the `sys` catalog
#'   views, capturing declared types (including IDENTITY), primary keys
#'   (composite keys included), nullability, defaults, and foreign keys
#'   (returned as [ForeignKey] objects, schema-qualified when the target lives
#'   in another schema).
reflect_columns.mssql <- function(x, tablename, ...) {
    conn <- x$get_connection()

    parts <- strsplit(tablename, "\\.")[[1]]
    if (length(parts) >= 2) {
        schema <- parts[1]
        table  <- parts[2]
    } else {
        schema <- DBI::dbGetQuery(conn, "SELECT SCHEMA_NAME() AS s")$s[1]
        table  <- parts[1]
    }
    sch_lit <- DBI::dbQuoteLiteral(conn, schema)
    tbl_lit <- DBI::dbQuoteLiteral(conn, table)

    cols <- tryCatch(
        DBI::dbGetQuery(conn, paste0(
            "SELECT c.name AS name, ty.name AS type_name, c.max_length AS max_length, ",
            "c.precision AS precision, c.scale AS scale, c.is_nullable AS is_nullable, ",
            "c.is_identity AS is_identity, ",
            # seed_value/increment_value are sql_variant; cast so ODBC returns
            # them as plain integers.
            "CAST(ic.seed_value AS BIGINT) AS seed_value, ",
            "CAST(ic.increment_value AS BIGINT) AS increment_value, ",
            "dc.definition AS default_expr ",
            "FROM sys.columns c ",
            "JOIN sys.tables t ON t.object_id = c.object_id ",
            "JOIN sys.schemas s ON s.schema_id = t.schema_id ",
            "JOIN sys.types ty ON ty.user_type_id = c.user_type_id ",
            "LEFT JOIN sys.identity_columns ic ",
            "ON ic.object_id = c.object_id AND ic.column_id = c.column_id ",
            "LEFT JOIN sys.default_constraints dc ON dc.object_id = c.default_object_id ",
            "WHERE s.name = ", sch_lit, " AND t.name = ", tbl_lit, " ",
            "ORDER BY c.column_id"
        )),
        error = function(e) {
            stop(
                sprintf("reflect: could not read table %s (%s).", tablename, conditionMessage(e)),
                call. = FALSE
            )
        }
    )

    if (NROW(cols) == 0) {
        stop(sprintf("reflect: table %s reports no columns.", tablename), call. = FALSE)
    }

    pk <- DBI::dbGetQuery(conn, paste0(
        "SELECT c.name AS name ",
        "FROM sys.indexes i ",
        "JOIN sys.index_columns ic ",
        "ON ic.object_id = i.object_id AND ic.index_id = i.index_id ",
        "JOIN sys.columns c ON c.object_id = ic.object_id AND c.column_id = ic.column_id ",
        "JOIN sys.tables t ON t.object_id = i.object_id ",
        "JOIN sys.schemas s ON s.schema_id = t.schema_id ",
        "WHERE i.is_primary_key = 1 AND s.name = ", sch_lit, " AND t.name = ", tbl_lit, " ",
        "ORDER BY ic.key_ordinal"
    ))$name

    fks <- DBI::dbGetQuery(conn, paste0(
        "SELECT pc.name AS local_col, rs.name AS ref_schema, rt.name AS ref_table, ",
        "rc.name AS ref_column, fk.delete_referential_action_desc AS on_delete, ",
        "fk.update_referential_action_desc AS on_update ",
        "FROM sys.foreign_keys fk ",
        "JOIN sys.foreign_key_columns fkc ON fkc.constraint_object_id = fk.object_id ",
        "JOIN sys.tables pt ON pt.object_id = fk.parent_object_id ",
        "JOIN sys.schemas ps ON ps.schema_id = pt.schema_id ",
        "JOIN sys.columns pc ",
        "ON pc.object_id = fkc.parent_object_id AND pc.column_id = fkc.parent_column_id ",
        "JOIN sys.tables rt ON rt.object_id = fk.referenced_object_id ",
        "JOIN sys.schemas rs ON rs.schema_id = rt.schema_id ",
        "JOIN sys.columns rc ",
        "ON rc.object_id = fkc.referenced_object_id AND rc.column_id = fkc.referenced_column_id ",
        "WHERE ps.name = ", sch_lit, " AND pt.name = ", tbl_lit
    ))

    fk_map <- stats::setNames(
        lapply(seq_len(NROW(fks)), function(i) fks[i, ]),
        as.character(fks$local_col)
    )

    build_field <- function(i) {
        nm <- cols$name[i]
        is_pk <- nm %in% pk
        args <- list(
            type = mssql_format_type(
                cols$type_name[i], cols$max_length[i], cols$precision[i], cols$scale[i],
                is_identity = isTRUE(as.logical(cols$is_identity[i])),
                seed = cols$seed_value[i], increment = cols$increment_value[i]
            ),
            # Pass TRUE for PKs, NULL otherwise: passing FALSE would trip
            # Column()'s `is.logical(primary_key)` branch and wipe nullable.
            primary_key = if (is_pk) TRUE else NULL,
            nullable = isTRUE(as.logical(cols$is_nullable[i]))
        )
        if (!is.na(cols$default_expr[i])) {
            args$default <- dbplyr::sql(cols$default_expr[i])
        }

        if (nm %in% names(fk_map)) {
            fk <- fk_map[[nm]]
            do.call(ForeignKey, c(
                list(
                    type       = args$type,
                    ref_schema = as.character(fk$ref_schema),
                    ref_table  = as.character(fk$ref_table),
                    ref_column = as.character(fk$ref_column),
                    on_delete  = mssql_fk_action(as.character(fk$on_delete)),
                    on_update  = mssql_fk_action(as.character(fk$on_update))
                ),
                args[setdiff(names(args), "type")]
            ))
        } else {
            do.call(Column, args)
        }
    }

    stats::setNames(
        lapply(seq_len(NROW(cols)), build_field),
        as.character(cols$name)
    )
}

#' @describeIn reflect_tables List base tables in a SQL Server schema
#'   (defaults to `SCHEMA_NAME()`).
reflect_tables.mssql <- function(x, .schema = NULL, ...) {
    conn <- x$get_connection()
    if (is.null(.schema)) {
        .schema <- DBI::dbGetQuery(conn, "SELECT SCHEMA_NAME() AS s")$s[1]
    }
    res <- DBI::dbGetQuery(conn, paste0(
        "SELECT t.name AS tablename FROM sys.tables t ",
        "JOIN sys.schemas s ON s.schema_id = t.schema_id ",
        "WHERE s.name = ", DBI::dbQuoteLiteral(conn, .schema),
        " ORDER BY t.name"
    ))
    as.character(res$tablename)
}
