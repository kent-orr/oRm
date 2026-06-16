#' @include Engine.R
#' @include TableModel.R
NULL

#' Augment an 'ellmer' Chat with database CRUD tools
#'
#' @description
#' Optional integration with the \pkg{ellmer} package. Given an ellmer
#' \code{Chat} object and one or more oRm models (or an \code{Engine}),
#' \code{register_db_tools()} registers a small set of generic CRUD tools onto
#' the chat so the LLM can read, create, update, and delete rows. The chat is
#' modified in place (ellmer Chat objects are reference objects) and returned
#' invisibly.
#'
#' @details
#' The registered tools are table-parameterised rather than model-specific:
#' \describe{
#'   \item{\code{db_read(table, filter)}}{Read rows. \code{filter} is an optional
#'     \pkg{dplyr}-style filter expression supplied as a string, e.g.
#'     \code{"age > 30 & name == 'Kent'"}. Results are returned as JSON.}
#'   \item{\code{db_create(table, values)}}{Insert a row. \code{values} is a JSON
#'     object of column/value pairs, e.g. \code{'{"name":"Kent","age":35}'}.}
#'   \item{\code{db_update(table, filter, values)}}{Update matching rows. A
#'     \code{filter} is required.}
#'   \item{\code{db_delete(table, filter)}}{Delete matching rows. A \code{filter}
#'     is required.}
#' }
#' The schema (tables and their columns) is embedded in each tool's description
#' so the LLM can discover what is available without an extra round-trip.
#'
#' Filter expressions are translated to SQL by \pkg{dbplyr} via oRm's existing
#' read/update/delete machinery, so the LLM can lean on its familiarity with
#' dplyr. Filters are parsed and evaluated in a minimal environment (a child of
#' \code{baseenv()}) so they cannot reach objects in the caller's workspace.
#'
#' Write permissions are governed entirely by the \code{Engine}: open it with
#' \code{.read_only = TRUE} to make \code{db_create}/\code{db_update}/
#' \code{db_delete} fail with the engine's read-only error (which is returned to
#' the LLM). There is no separate permission layer here. Because the agent acts
#' with the caller's database privileges, prefer a read-only engine when exposing
#' a chat to untrusted input.
#'
#' @param chat An ellmer \code{Chat} object (e.g. from \code{ellmer::chat_anthropic()}).
#' @param ... One or more \code{TableModel} objects to expose, and/or a single
#'   \code{Engine} whose tables are reflected automatically via
#'   \code{\link{Engine}}'s \code{reflect_schema()}.
#' @param tables Optional character vector limiting which tables to reflect when
#'   an \code{Engine} is supplied. Ignored for explicit \code{TableModel}s.
#' @param exclude Optional character vector of table names to skip when an
#'   \code{Engine} is supplied.
#' @param max_rows Integer. Maximum number of rows \code{db_read} returns to the
#'   LLM. Defaults to 100.
#'
#' @return The \code{chat} object, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ellmer)
#' engine <- Engine$new(RSQLite::SQLite(), dbname = ":memory:", persist = TRUE)
#' User <- engine$model(
#'     "users",
#'     id = Column("INTEGER", primary_key = TRUE),
#'     name = Column("TEXT"),
#'     age = Column("INTEGER")
#' )
#' User$create_table()
#'
#' chat <- chat_anthropic()
#' register_db_tools(chat, User)
#' chat$chat("Add a user named Kent who is 35, then list everyone over 30.")
#' }
register_db_tools <- function(chat, ..., tables = NULL, exclude = NULL, max_rows = 100) {
    if (!requireNamespace("ellmer", quietly = TRUE)) {
        stop(
            "register_db_tools() requires the 'ellmer' package. ",
            "Install it with install.packages('ellmer').",
            call. = FALSE
        )
    }
    if (!inherits(chat, "Chat")) {
        stop("`chat` must be an ellmer Chat object.", call. = FALSE)
    }

    registry <- build_model_registry(..., tables = tables, exclude = exclude)
    if (length(registry) == 0) {
        stop(
            "No tables to expose. Pass one or more TableModel objects and/or an Engine.",
            call. = FALSE
        )
    }

    schema_desc <- schema_description(registry)
    table_names <- names(registry)

    chat$register_tools(list(
        db_read_tool(registry, schema_desc, table_names, max_rows),
        db_create_tool(registry, schema_desc, table_names),
        db_update_tool(registry, schema_desc, table_names),
        db_delete_tool(registry, schema_desc, table_names)
    ))

    invisible(chat)
}

# Collect TableModels (explicit) and reflected Engine tables into a named
# registry keyed by bare (unqualified) table name.
build_model_registry <- function(..., tables = NULL, exclude = NULL) {
    dots <- list(...)
    registry <- list()
    for (obj in dots) {
        if (inherits(obj, "TableModel")) {
            registry[[bare_tablename(obj)]] <- obj
        } else if (inherits(obj, "Engine")) {
            models <- obj$reflect_schema(tables = tables, exclude = exclude)
            for (nm in names(models)) {
                registry[[nm]] <- models[[nm]]
            }
        } else {
            stop(
                "register_db_tools() accepts TableModel objects and/or an Engine; got ",
                paste(class(obj), collapse = "/"), ".",
                call. = FALSE
            )
        }
    }
    registry
}

# Last segment of a possibly schema-qualified table name (e.g. "app.users" -> "users").
bare_tablename <- function(model) {
    parts <- strsplit(model$tablename, "\\.")[[1]]
    parts[length(parts)]
}

# Human-readable enumeration of tables and their columns for tool descriptions.
schema_description <- function(registry) {
    lines <- vapply(names(registry), function(nm) {
        model <- registry[[nm]]
        cols <- vapply(names(model$fields), function(cn) {
            paste(cn, model$fields[[cn]]$type)
        }, character(1))
        sprintf("- %s(%s)", nm, paste(cols, collapse = ", "))
    }, character(1))
    paste(lines, collapse = "\n")
}

# Look up a model by name, erroring with the valid set for a helpful LLM retry.
get_model <- function(registry, table) {
    model <- registry[[table]]
    if (is.null(model)) {
        stop(
            "Unknown table '", table, "'. Available tables: ",
            paste(names(registry), collapse = ", "), ".",
            call. = FALSE
        )
    }
    model
}

# Parse a dplyr-style filter string into a quosure bound to a minimal
# environment, so the LLM's expression cannot reach the caller's objects.
parse_filter <- function(filter) {
    expr <- rlang::parse_expr(filter)
    rlang::new_quosure(expr, env = rlang::new_environment(parent = baseenv()))
}

# Parse a JSON object of column/value pairs into a named list.
parse_values <- function(values) {
    if (is.null(values) || !nzchar(values)) {
        stop("`values` must be a JSON object of column/value pairs.", call. = FALSE)
    }
    parsed <- tryCatch(
        jsonlite::fromJSON(values, simplifyVector = TRUE),
        error = function(e) stop("Could not parse `values` as JSON: ", conditionMessage(e), call. = FALSE)
    )
    as.list(parsed)
}

db_read_tool <- function(registry, schema_desc, table_names, max_rows) {
    fun <- function(table, filter = NULL) {
        as.character(tryCatch({
            model <- get_model(registry, table)
            if (is.null(filter) || !nzchar(filter)) {
                rows <- model$read(.mode = "data.frame", .limit = max_rows)
            } else {
                q <- parse_filter(filter)
                rows <- model$read(!!q, .mode = "data.frame", .limit = max_rows)
            }
            jsonlite::toJSON(rows, dataframe = "rows", na = "null", auto_unbox = TRUE)
        }, error = function(e) paste("Error:", conditionMessage(e))))
    }
    ellmer::tool(
        fun,
        description = paste0(
            "Read rows from a database table. `filter` is an optional dplyr-style ",
            "filter expression as a string, e.g. \"age > 30 & name == 'Kent'\". ",
            "Returns up to ", max_rows, " rows as JSON.\n\n",
            "Available tables and columns:\n", schema_desc
        ),
        arguments = list(
            table = ellmer::type_enum(table_names, "The table to read from."),
            filter = ellmer::type_string("Optional dplyr filter expression.", required = FALSE)
        ),
        name = "db_read"
    )
}

db_create_tool <- function(registry, schema_desc, table_names) {
    fun <- function(table, values) {
        as.character(tryCatch({
            model <- get_model(registry, table)
            model$create(.data = parse_values(values))
            paste0("Inserted 1 row into ", table, ".")
        }, error = function(e) paste("Error:", conditionMessage(e))))
    }
    ellmer::tool(
        fun,
        description = paste0(
            "Insert a row into a database table. `values` is a JSON object of ",
            "column/value pairs, e.g. '{\"name\":\"Kent\",\"age\":35}'.\n\n",
            "Available tables and columns:\n", schema_desc
        ),
        arguments = list(
            table = ellmer::type_enum(table_names, "The table to insert into."),
            values = ellmer::type_string("JSON object of column/value pairs.")
        ),
        name = "db_create"
    )
}

db_update_tool <- function(registry, schema_desc, table_names) {
    fun <- function(table, filter, values) {
        as.character(tryCatch({
            model <- get_model(registry, table)
            if (is.null(filter) || !nzchar(filter)) {
                stop("`filter` is required for updates.", call. = FALSE)
            }
            n <- model$update(!!parse_filter(filter), .data = parse_values(values))
            paste0("Updated ", n, " row(s) in ", table, ".")
        }, error = function(e) paste("Error:", conditionMessage(e))))
    }
    ellmer::tool(
        fun,
        description = paste0(
            "Update rows in a database table. `filter` is a required dplyr-style ",
            "filter expression selecting the rows to change; `values` is a JSON ",
            "object of the columns to set.\n\n",
            "Available tables and columns:\n", schema_desc
        ),
        arguments = list(
            table = ellmer::type_enum(table_names, "The table to update."),
            filter = ellmer::type_string("dplyr filter expression selecting rows to update."),
            values = ellmer::type_string("JSON object of columns to set.")
        ),
        name = "db_update"
    )
}

db_delete_tool <- function(registry, schema_desc, table_names) {
    fun <- function(table, filter) {
        as.character(tryCatch({
            model <- get_model(registry, table)
            if (is.null(filter) || !nzchar(filter)) {
                stop("`filter` is required for deletes.", call. = FALSE)
            }
            n <- model$delete(!!parse_filter(filter))
            paste0("Deleted ", n, " row(s) from ", table, ".")
        }, error = function(e) paste("Error:", conditionMessage(e))))
    }
    ellmer::tool(
        fun,
        description = paste0(
            "Delete rows from a database table. `filter` is a required dplyr-style ",
            "filter expression selecting the rows to delete.\n\n",
            "Available tables and columns:\n", schema_desc
        ),
        arguments = list(
            table = ellmer::type_enum(table_names, "The table to delete from."),
            filter = ellmer::type_string("dplyr filter expression selecting rows to delete.")
        ),
        name = "db_delete"
    )
}
