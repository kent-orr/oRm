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
