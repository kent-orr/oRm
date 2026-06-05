#' @describeIn check_schema_exists Check if a schema exists for PostgreSQL.
check_schema_exists.postgres <- function(x, .schema) {
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

  sql <- paste0("SELECT 1 FROM pg_namespace WHERE nspname = ", DBI::dbQuoteLiteral(conn, .schema))
  exists <- FALSE
  try({
    res <- DBI::dbGetQuery(conn, sql)
    exists <- NROW(res) > 0
  }, silent = TRUE)
  exists
}

#' @rdname flush
#' @usage \method{flush}{postgres}(x, table, data, con, commit = TRUE, ...)
#' @description Insert a row and return the inserted record using PostgreSQL's RETURNING clause.
flush.postgres <- function(x, table, data, con, commit = TRUE, ...) {
  # Build the insert SQL
  data <- data[!vapply(data, is.null, logical(1))]
  tbl_expr <- dbplyr::ident_q(table)
  fields <- names(data)

  # Extract field definitions if provided
  dots <- list(...)
  field_defs <- dots$fields

  # Create a map of field names to types for JSON detection
  json_fields <- character(0)
  if (!is.null(field_defs)) {
    json_fields <- names(field_defs)[vapply(field_defs, function(f) {
      toupper(f$type) %in% c("JSON", "JSONB")
    }, logical(1))]
  }

  # Handle empty data case - insert with DEFAULT VALUES
  if (length(fields) == 0) {
    sql <- paste0("INSERT INTO ", tbl_expr, " DEFAULT VALUES RETURNING *")
  } else {
    # Convert data values to proper format for SQL, handling Date/POSIXct/JSON objects
    formatted_values <- lapply(names(data), function(field_name) {
      x <- data[[field_name]]

      # Check if this is a JSON column that needs serialization
      if (field_name %in% json_fields) {
        # Serialize unless this is already valid JSON text.
        if (is.character(x) && length(x) == 1) {
          if (jsonlite::validate(x)) {
            x
          } else {
            jsonlite::toJSON(x, auto_unbox = TRUE)
          }
        } else {
          # Serialize R objects to JSON (vectors, lists, etc.)
          # auto_unbox=TRUE: scalars in lists stay scalar, vectors stay as arrays
          jsonlite::toJSON(x, auto_unbox = TRUE)
        }
      } else if (inherits(x, "Date")) {
        as.character(x)
      } else if (inherits(x, "POSIXt")) {
        format(x, "%Y-%m-%d %H:%M:%S")
      } else {
        x
      }
    })

    # Unlist to convert list to vector for dbQuoteLiteral
    formatted_values <- unlist(formatted_values, use.names = FALSE)
    values_sql <- paste0("(", paste(DBI::dbQuoteLiteral(con, formatted_values), collapse = ", "), ")")
    field_sql <- paste(DBI::dbQuoteIdentifier(con, fields), collapse = ", ")

    sql <- paste0(
      "INSERT INTO ", tbl_expr, " (", field_sql, ") VALUES ", values_sql,
      " RETURNING *"
    )
  }

  # Just execute the query and return the result
  # Let the caller handle transaction management
  result <- DBI::dbGetQuery(con, sql)

  # Deserialize JSON/JSONB columns in the returned result
  if (nrow(result) > 0 && length(json_fields) > 0) {
    for (json_field in json_fields) {
      if (json_field %in% names(result)) {
        result[[json_field]] <- lapply(result[[json_field]], function(val) {
          if (is.null(val) || is.na(val)) {
            return(val)
          }
          # Parse JSON string back to R object
          tryCatch({
            jsonlite::fromJSON(val, simplifyVector = TRUE, simplifyDataFrame = FALSE)
          }, error = function(e) {
            # If parsing fails, return original value
            val
          })
        })
      }
    }
  }

  return(result)
}

#' @describeIn qualify Add the schema prefix to unqualified table names for PostgreSQL.
qualify.postgres <- function(x, tablename, .schema) {
  if (!grepl("\\.", tablename) && !is.null(.schema)) {
    paste(.schema, tablename, sep = ".")
  } else {
    tablename
  }
}

#' @describeIn set_schema PostgreSQL applies schema via search_path.
set_schema.postgres <- function(x, .schema) {
    if (is.null(.schema)) return(invisible(NULL))

    # Get the engine object
    engine <- if (inherits(x, "Engine")) x else x$engine

    # Access connection directly to avoid recursion (get_connection calls set_schema)
    # If connection is invalid, we can't set schema - it should be set during next get_connection
    conn <- engine$conn

    # Defensive check: ensure connection is not NULL before using it
    if (is.null(conn)) {
        return(invisible(NULL))
    }

    # Check if connection is valid
    if (!DBI::dbIsValid(conn)) {
        return(invisible(NULL))
    }

    sql <- paste0("SET search_path TO ", DBI::dbQuoteIdentifier(conn, .schema))
    execute_sql(x, conn, sql)

    invisible(NULL)
}

#' @describeIn create_schema Create the schema for PostgreSQL.
#'   Suppresses notices when the schema already exists.
create_schema.postgres <- function(x, .schema) {
    if (is.null(.schema)) stop("Must supply a schema name.", call. = FALSE)
    
    # Get a direct connection without triggering schema setting
    conn <- NULL
    engine <- if (inherits(x, "Engine")) x else x$engine
    
    if (is.null(engine$conn) || !DBI::dbIsValid(engine$conn)) {
        conn <- do.call(DBI::dbConnect, engine$conn_args)
        on.exit(DBI::dbDisconnect(conn), add = TRUE)
    } else {
        conn <- engine$conn
    }
    
    sql <- paste0("CREATE SCHEMA IF NOT EXISTS ", DBI::dbQuoteIdentifier(conn, .schema))
    suppressMessages(DBI::dbExecute(conn, sql))
    invisible(TRUE)
}

#' @describeIn execute_sql Suppress PostgreSQL messages when executing SQL commands.
execute_sql.postgres <- function(x, con, sql) {
    suppressMessages(DBI::dbExecute(con, sql))
}

#' @describeIn apply_read_only PostgreSQL enforces read-only via the libpq
#'   `options="-c default_transaction_read_only=on"` connection parameter
#'   injected into `conn_args` at engine construction; nothing more to do
#'   post-connect.
apply_read_only.postgres <- function(x, con) {
    invisible(NULL)
}


# Reflection -------------------------------------------------------------

#' Map a PostgreSQL referential-action code to a SQL action.
#'
#' `pg_constraint.confdeltype`/`confupdtype` are single characters. "NO ACTION"
#' (`a`) is the implicit default and is returned as NULL so it is not rendered.
#' @keywords internal
pg_fk_action <- function(code) {
    switch(code,
        a = NULL,        # NO ACTION (default)
        r = "RESTRICT",
        c = "CASCADE",
        n = "SET NULL",
        d = "SET DEFAULT",
        NULL
    )
}

#' @describeIn reflect_columns PostgreSQL reflection via `pg_catalog`, capturing
#'   canonical types, primary keys, nullability, defaults, and foreign keys
#'   (returned as [ForeignKey] objects, schema-qualified when the target lives
#'   in another schema).
reflect_columns.postgres <- function(x, tablename, ...) {
    conn <- x$get_connection()

    parts <- strsplit(tablename, "\\.")[[1]]
    if (length(parts) >= 2) {
        schema <- parts[1]
        table  <- parts[2]
    } else {
        schema <- DBI::dbGetQuery(conn, "SELECT current_schema() AS s")$s[1]
        table  <- parts[1]
    }
    sch_lit <- DBI::dbQuoteLiteral(conn, schema)
    tbl_lit <- DBI::dbQuoteLiteral(conn, table)

    cols <- tryCatch(
        DBI::dbGetQuery(conn, paste0(
            "SELECT a.attname AS name, ",
            "pg_catalog.format_type(a.atttypid, a.atttypmod) AS type, ",
            "a.attnotnull AS notnull, ",
            "pg_catalog.pg_get_expr(d.adbin, d.adrelid) AS default_expr ",
            "FROM pg_catalog.pg_attribute a ",
            "JOIN pg_catalog.pg_class c ON c.oid = a.attrelid ",
            "JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace ",
            "LEFT JOIN pg_catalog.pg_attrdef d ON d.adrelid = a.attrelid AND d.adnum = a.attnum ",
            "WHERE n.nspname = ", sch_lit, " AND c.relname = ", tbl_lit, " ",
            "AND a.attnum > 0 AND NOT a.attisdropped ",
            "ORDER BY a.attnum"
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
        "SELECT a.attname AS name ",
        "FROM pg_catalog.pg_constraint con ",
        "JOIN pg_catalog.pg_class c ON c.oid = con.conrelid ",
        "JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace ",
        "JOIN pg_catalog.pg_attribute a ON a.attrelid = con.conrelid AND a.attnum = ANY(con.conkey) ",
        "WHERE con.contype = 'p' AND n.nspname = ", sch_lit, " AND c.relname = ", tbl_lit
    ))$name

    fks <- DBI::dbGetQuery(conn, paste0(
        "SELECT la.attname AS local_col, fn.nspname AS ref_schema, ",
        "fc.relname AS ref_table, fa.attname AS ref_column, ",
        "con.confdeltype AS on_delete, con.confupdtype AS on_update ",
        "FROM pg_catalog.pg_constraint con ",
        "JOIN pg_catalog.pg_class c ON c.oid = con.conrelid ",
        "JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace ",
        "JOIN pg_catalog.pg_class fc ON fc.oid = con.confrelid ",
        "JOIN pg_catalog.pg_namespace fn ON fn.oid = fc.relnamespace ",
        "JOIN generate_subscripts(con.conkey, 1) AS k(i) ON true ",
        "JOIN pg_catalog.pg_attribute la ON la.attrelid = con.conrelid AND la.attnum = con.conkey[k.i] ",
        "JOIN pg_catalog.pg_attribute fa ON fa.attrelid = con.confrelid AND fa.attnum = con.confkey[k.i] ",
        "WHERE con.contype = 'f' AND n.nspname = ", sch_lit, " AND c.relname = ", tbl_lit
    ))

    fk_map <- stats::setNames(
        lapply(seq_len(NROW(fks)), function(i) fks[i, ]),
        as.character(fks$local_col)
    )

    build_field <- function(i) {
        nm <- cols$name[i]
        is_pk <- nm %in% pk
        args <- list(
            type = cols$type[i],
            # Pass TRUE for PKs, NULL otherwise: passing FALSE would trip
            # Column()'s `is.logical(primary_key)` branch and wipe nullable.
            primary_key = if (is_pk) TRUE else NULL,
            nullable = !isTRUE(cols$notnull[i])
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
                    on_delete  = pg_fk_action(as.character(fk$on_delete)),
                    on_update  = pg_fk_action(as.character(fk$on_update))
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

#' @describeIn reflect_tables List base tables in a PostgreSQL schema
#'   (defaults to `current_schema()`).
reflect_tables.postgres <- function(x, .schema = NULL, ...) {
    conn <- x$get_connection()
    if (is.null(.schema)) {
        .schema <- DBI::dbGetQuery(conn, "SELECT current_schema() AS s")$s[1]
    }
    res <- DBI::dbGetQuery(conn, paste0(
        "SELECT tablename FROM pg_catalog.pg_tables WHERE schemaname = ",
        DBI::dbQuoteLiteral(conn, .schema),
        " ORDER BY tablename"
    ))
    as.character(res$tablename)
}
