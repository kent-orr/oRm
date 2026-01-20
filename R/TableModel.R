#' @include Dialect.R
NULL

#' TableModel Class
#'
#' @description
#' The TableModel class represents a database table in the oRm framework. It manages
#' table structure, fields, relationships, and provides methods for interacting
#' with the database table.
#'
#' @details
#' TableModel is a core component of the oRm framework, responsible for:
#' \itemize{
#'     \item Defining table structure with columns and relationships
#'     \item Creating and managing database tables
#'     \item Providing an interface for CRUD operations on table records
#'     \item Managing relationships between different tables
#' }
#'
#' Key features:
#' \itemize{
#'     \item Dynamic table creation and management
#'     \item Support for various column types and constraints
#'     \item Relationship definitions and querying
#'     \item Record creation and retrieval
#' }
#'
#' @section Methods:
#' \describe{
#'     \item{\code{initialize(tablename, engine, ..., .data = list(), .schema = NULL, .default_mode = "all")}}{Constructor for creating a new TableModel instance.}
#'     \item{\code{get_connection()}}{Retrieve the active database connection from the engine.}
#'     \item{\code{generate_sql_fields()}}{Generate SQL field definitions for table creation.}
#'     \item{\code{create_table(if_not_exists = TRUE, overwrite = FALSE, verbose = FALSE)}}{Create the associated table in the database.}
#'     \item{\code{record(..., .data = list())}}{Create a new Record object associated with this model.}
#'     \item{\code{read(..., .mode = NULL, .limit = NULL)}}{Read records from the table using dynamic filters. If `.mode` is NULL, uses `default_mode`.}
#'     \item{\code{relationship(rel_name, ...)}}{Query related records based on defined relationships.}
#'     \item{\code{print()}}{Print a concise summary of the model, including its fields.}
#' }
#'
#' @seealso \code{\link{Engine}}, \code{\link{Record}}, \code{\link{Column}}, \code{\link{ForeignKey}}
#'
#' @importFrom R6 R6Class
#' @importFrom dplyr tbl filter slice_head slice_tail collect
#' @importFrom rlang enquos
#' @importFrom DBI dbQuoteIdentifier dbQuoteLiteral
#'
#' @field tablename Fully qualified name of the table in the database.
#' @field schema Schema that namespaces the table; defaults to the engine's schema.
#' @field engine Engine instance providing connections and SQL dialect.
#' @field fields Named list of Column objects defining the table structure.
#' @field relationships Named list of Relationship objects linking to other models.
#' @field default_mode Default mode for reading records when `.mode` is NULL.
#' 
#' @export
#' 
TableModel <- R6::R6Class(
    "TableModel",
    public = list(
    tablename = NULL,
    schema = NULL,
    engine = NULL,
    fields = list(),
    relationships = list(),
    methods = list(),
    default_mode = "all",

    #' @description
    #' Constructor for a new TableModel.
    #' @param tablename The name of the database table.
    #' @param engine The Engine object for database connection.
    #' @param ... Column definitions.
    #' @param .data a list of Column defintions
    #' @param .schema Character. Schema to apply to the table name. Defaults to the engine's schema.
        #' @param .default_mode Character. Default mode used when `read()` is called with `.mode` = NULL. Must be one of "all", "one_or_none", "get", "data.frame", or "tbl".
    initialize = function(tablename, engine, ..., .data = list(), .schema = NULL, .default_mode = c("all", "one_or_none", "get", "data.frame", "tbl")) {
        if (missing(tablename) || missing(engine)) {
            stop("Both 'tablename' and 'engine' must be provided to TableModel.")
        }


        self$engine <- engine
        if (is.null(.schema)) {
            self$schema <- self$engine$schema
        } else {
            self$schema <- .schema
        }
        self$tablename <- qualify(engine, tablename, .schema = self$schema)
        .default_mode <- match.arg(.default_mode)
        self$default_mode <- .default_mode

        dots <- utils::modifyList(.data, rlang::list2(...))
        col_defs <- dots[vapply(dots, inherits, logical(1), "Column")]
        col_names <- names(dots[vapply(dots, inherits, logical(1), "Column")])
        for (i in seq_along(col_defs)) {
            col_defs[[i]][['name']] <- col_names[i]
            class(col_defs[[i]]) <- append(class(col_defs[[i]]), engine$dialect)
        }
        self$fields <- col_defs

        # Extract and process Method objects
        method_defs <- dots[vapply(dots, inherits, logical(1), "orm_method")]
        method_names <- names(dots[vapply(dots, inherits, logical(1), "orm_method")])

        # Assign names to methods and separate by target
        for (i in seq_along(method_defs)) {
            method_defs[[i]][['name']] <- method_names[i]
        }
        self$methods <- method_defs

        # Inject table-targeted methods into this TableModel instance
        for (i in seq_along(method_defs)) {
            method <- method_defs[[i]]
            if (method$target == "table") {
                # Inject method into this R6 instance
                method_name <- method$name
                self[[method_name]] <- method$fn
            }
        }
    },

    #' @description
    #' Retrieve the active database connection from the engine.
    #' Delegates to the associated engine and respects schema and
    #' pooling settings.
    #' @param ... Additional arguments passed to the engine's
    #'     `get_connection` method.
    #' @seealso Engine::get_connection
    get_connection = function(...) {
        self$engine$get_connection(...)
    },


    #' @description
    #' Update the schema for this model and re-qualify the table name.
    #' @param .schema Character. New schema name to apply.
    #' @return The TableModel object.
    set_schema = function(.schema) {
        self$schema <- .schema
        base_name <- strsplit(self$tablename, "\\.")[[1]]
        base_name <- base_name[length(base_name)]
        self$tablename <- qualify(self$engine, base_name, .schema = .schema)
        self
    },

    #' @description
    #' Create the associated table in the database.
    #' @param if_not_exists Logical. If TRUE, only create the table if it doesn't exist. Default is TRUE.
    #' @param overwrite Logical. If TRUE, drop the table if it exists and recreate it. Default is FALSE.
    #' @param verbose Logical. If TRUE, return the SQL statement instead of executing it. Default is FALSE.
    #' @return The TableModel object invisibly.
    #' @note Errors if the table's schema does not exist.
    create_table = function(if_not_exists = TRUE, overwrite = FALSE, verbose = FALSE) {
        if (!is.null(self$schema)) {
            if (!check_schema_exists(self$engine, self$schema)) {
                stop(
                    sprintf(
                        "Schema '%s' does not exist. Create it using engine$create_schema('%s') before proceeding.",
                        self$schema, self$schema
                    ),
                    call. = FALSE
                )
            }
        }

        if (overwrite) {
            conn <- self$get_connection()
            drop_sql <- paste0("DROP TABLE IF EXISTS ", DBI::dbQuoteIdentifier(conn, self$tablename))
            if (verbose) {
                cat(drop_sql, "\n")
            } else {
                self$engine$execute(drop_sql)
            }
        }
        
        # Get fresh connection after potential execute() call that may close it
        conn <- self$get_connection()
        fields_sql = c()
        constraints_sql = c()
        for (i in seq_along(self$fields)) {
            field_name = names(self$fields)[i]
            field = self$fields[[i]]
            fields_sql = c(fields_sql, render_field(field, conn))
            constraints_sql = c(constraints_sql, render_constraint(field, conn))
        }

        create_clause <- if (if_not_exists) "CREATE TABLE IF NOT EXISTS" else "CREATE TABLE"
        sql <- paste0(
            create_clause, " ",
            self$engine$format_tablename(self$tablename), 
                " (\n    ", paste(fields_sql, collapse = ',\n'), 
                if (length(constraints_sql) > 0 && any(constraints_sql != "")) {
                paste0(",\n    ", paste(constraints_sql[constraints_sql != ""], collapse = ",\n    "))
                } else {
                ""
            },
            "\n);\n"
        )

        if (verbose) {
        return(sql)
        }

        self$engine$execute(sql)
        return(self)
    },

    #' @description
    #' Drop the associated table from the database. Prompts for confirmation
    #' by default if running interactively.
    #'
    #' @param ask Logical. If TRUE (default in interactive sessions), prompts
    #' the user for confirmation before dropping the table.
    #'
    #' @return Invisibly returns the result of `DBI::dbExecute()` if the table is dropped,
    #' or `NULL` if the operation is canceled or skipped.
    #'
    #' @examples
    #' \donttest{
    #' # Drop the "users" table after confirmation
    #' User$drop_table()
    #'
    #' # Force drop without confirmation
    #' User$drop_table(ask = FALSE)
    #' }
    drop_table = function(ask = interactive()) {
        drop_sql <- paste0("DROP TABLE IF EXISTS ", self$engine$format_tablename(self$tablename))

        resp <- 'y'
        if (ask) {
        resp <- readline(paste0("Are you sure you want to drop ", self$tablename, "? [y/N] "))
        }

        if (grepl('y', resp, ignore.case = TRUE)) {
        self$engine$execute(drop_sql)
        } else {
        message("Table not dropped.")
        return(invisible(NULL))
        }
    },


    #' @description
    #' Create a new Record object with this model.
    #' @param ... Named values to initialize the record's data.
    #' @param .data a named list of field values.
    #'
    record = function(..., .data = list()) {
        Record$new(self, ..., .data = .data)
    },

    #' @description
    #' Generate a dbplyr tbl() object to be consumed by the model.
    tbl = function() {
        con = self$get_connection()
        formatted_name <- self$engine$format_tablename(self$tablename)
        dplyr::tbl(con, I(formatted_name))
    },

    #' @description
    #' Read records using dynamic filters and return in the specified mode.
    #' @param ... Unquoted expressions for filtering.
    #' @param .mode Mode for reading records. One of "all", "one_or_none", "get", "data.frame", or "tbl". If NULL, uses `default_mode`.
    #'     "data.frame" returns the raw result of `dplyr::collect()` rather than Record objects.
    #'     "tbl" returns the uncollected dbplyr table.
    #' @param .limit Integer. Maximum number of records to return. Defaults to 100. NULL means no limit.
    #'     Positive values return the first N records, negative values return the last N records.
    #' @param .offset Integer. Offset for pagination. Default is 0.
    #' @param .order_by Unquoted expressions for ordering. Defaults to NULL (no order). Calls dplyr::arrange() so can take multiple args / desc()
    read = function(
        ...,
        .mode = NULL,
        .limit = 100,
        .offset=0,
        .order_by = list()
    ) {

        if (is.null(.mode)) {
        .mode <- self$default_mode
        }
        .mode <- match.arg(.mode, c("all", "one_or_none", "get", "data.frame", "tbl"))
        tbl_ref <- self$tbl()

        if (.mode == "tbl" && missing(.limit)) {
        .limit <- NULL
        }

        filters <- rlang::enquos(...)
        if (length(filters) > 0) {
        tbl_ref <- dplyr::filter(tbl_ref, !!!filters)
        }

        # Apply ordering (only if user provided .order_by)
        if (!missing(.order_by)) {
        order_exprs <- rlang::enexpr(.order_by)
        
        # Handle .order_by = c(x, desc(y)) vs .order_by = x
        if (rlang::is_call(order_exprs, "c")) {
            order_exprs <- as.list(order_exprs)[-1]    # Drop the "c" call
        } else {
            order_exprs <- list(order_exprs)
        }
        
        tbl_ref <- dplyr::arrange(tbl_ref, !!!order_exprs)
        }

        # Apply pagination using SQL-compatible operations
        if (!is.null(.limit) && is.numeric(.limit) && .limit != 0) {
        if (.limit > 0) {
            # For positive limits with offset
            if (.offset > 0) {
            # SQL databases support LIMIT and OFFSET directly
            tbl_ref <- tbl_ref |> 
                dplyr::mutate(.row_id = dplyr::row_number()) |>
                dplyr::filter(.row_id > .offset & .row_id <= .offset + .limit) |>
                dplyr::select(-.row_id)
            } else {
            # Just limit without offset
            tbl_ref <- tbl_ref |>
                dplyr::mutate(.row_id = dplyr::row_number()) |>
                dplyr::filter(.row_id <= .limit) |>
                dplyr::select(-.row_id)
            }
        } else {
            # For negative limits (last N rows)
            tbl_ref <- tbl_ref |>
            dplyr::mutate(.row_id = dplyr::row_number()) |>
            dplyr::filter(.row_id > dplyr::n() - abs(.limit)) |>
            dplyr::select(-.row_id)
        }
        } else if (.offset > 0) {
        # Just offset without limit
        tbl_ref <- tbl_ref |>
            dplyr::mutate(.row_id = dplyr::row_number()) |>
            dplyr::filter(.row_id > .offset) |>
            dplyr::select(-.row_id)
        }

        if (.mode == "tbl") return(tbl_ref)

        rows <- dplyr::collect(tbl_ref)

        # Deserialize JSON columns if this is a PostgreSQL database
        if (self$engine$dialect == "postgres" && nrow(rows) > 0) {
            # Identify JSON/JSONB columns
            json_fields <- names(self$fields)[vapply(self$fields, function(f) {
                toupper(f$type) %in% c("JSON", "JSONB")
            }, logical(1))]

            # Deserialize each JSON column
            for (json_field in json_fields) {
                if (json_field %in% names(rows)) {
                    rows[[json_field]] <- lapply(rows[[json_field]], function(val) {
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

        if (.mode == "data.frame") {
        return(rows)
        }

        if (nrow(rows) == 0) {
        if (.mode == "get") {
            stop("Expected exactly one row, got: 0")
        } else if (.mode == "one_or_none") {
            return(NULL)
        } else if (.mode == "all") {
            return(list())
        }
        }
        create_record <- function(row_data) {
            Record$new(model = self, .data = as.list(row_data))
        }

        if (.mode == "get") {
        if (nrow(rows) != 1) stop("Expected exactly one row, got: ", nrow(rows))
        return(create_record(rows[1, , drop = TRUE]))
        }

        if (.mode == "one_or_none") {
        if (nrow(rows) > 1) stop("Expected zero or one row, got multiple")
        if (nrow(rows) == 1) return(create_record(rows[1, , drop = TRUE]))
        return(NULL)
        }

        # .mode == "all"
        lapply(seq_len(nrow(rows)), function(i) create_record(rows[i, , drop = TRUE]))
    },

    #' @description
    #' Shortcut for retrieving a single record. Expects exactly one matching record.
    #' @param ... Unquoted expressions for filtering. For models with a single primary key, 
    #'     the first unnamed argument can be the primary key value.
    #' @return A Record object if exactly one record is found.
    #' @note Throws an error if zero or multiple records are found.
    #' @examples
    #' \donttest{
    #' # Get by primary key value (if single primary key exists)
    #' user <- User$get(123)
    #' 
    #' # Get by named filter
    #' user <- User$get(email == "user@example.com")
    #' 
    #' # Get with multiple filters
    #' user <- User$get(name == "John", active == 1)
    #' }
    get = function(...) {
        self$read(...,.mode = "get")
    },    

    #' @description
    #' Retrieve all records matching the given filters.
    #' @param ... Unquoted expressions for filtering.
    #' @return A list of Record objects, or NULL if no records are found.
    #' @examples
    #' \donttest{
    #' # Get all users
    #' users <- User$all()
    #' 
    #' # Get all active users
    #' active_users <- User$all(active == 1)
    #' 
    #' # Get users with specific criteria
    #' admin_users <- User$all(role == "admin", active == 1)
    #' }
    all = function(...) {
        self$read(...,.mode = "all")
    },

    #' @description
    #' Retrieve zero or one record matching the given filters.
    #' @param ... Unquoted expressions for filtering.
    #' @return A Record object if exactly one record is found, NULL if no records are found.
    #' @note Throws an error if multiple records are found.
    #' @examples
    #' \donttest{
    #' # Get user by email (expects 0 or 1 result)
    #' user <- User$one_or_none(email == "user@example.com")
    #' 
    #' # Returns NULL if no match
    #' user <- User$one_or_none(id == 999)
    #' 
    #' # Errors if multiple matches
    #' user <- User$one_or_none(status == "active")  # Error if multiple active users
    #' }
    one_or_none = function(...) {
        self$read(...,.mode = "one_or_none")
    },

    
      


    #' Retrieve related records based on a defined relationship.
    #'
    #' @details
    #' This method returns related records based on the relationship type:
    #' - For 'belongs_to', 'owns', 'one_to_one', and 'many_to_one' relationships, it returns a single Record object or NULL.
    #' - For 'one_to_many' and 'many_to_many' relationships, it returns a list of Record objects.
    #'
    #' For per-record filtering based on existing data, use [Record$relationship()], which applies additional constraints.
    #'
    #' @param rel_name The name of the relationship to query.
    #' @param ... Additional arguments passed to the related model's read method.
    #' @return A single Record, a list of Records, or NULL, depending on the relationship type.
    #' @seealso [Record$relationship()]
    relationship = function(rel_name, ...) {
        if (!rel_name %in% names(self$relationships)) stop("Invalid relationship name: ", rel_name)

        rel <- self$relationships[[rel_name]]
        if (!inherits(rel, "Relationship")) stop("Invalid relationship: ", rel_name)

        mode <- switch(rel$type,
        "belongs_to" = "one_or_none",
        "owns" = "one_or_none",
        "one_to_one" = "one_or_none",
        "one_to_many" = "all",
        "many_to_many" = "all",
        "many_to_one" = "one_or_none",
        stop("Unknown relationship type: ", rel$type)
        )

        rel$related_model$read(..., .mode = mode)

    },

    #' @description
    #' Print a concise summary of the model, including the table name and
    #' column names.
    #' @param ... Unused, present for compatibility.
    #' @return The TableModel object, invisibly.
    #' @seealso [Engine$print()], [Record$print()].
    print = function(...) {
        cat("<TableModel>\n")
        cat("Table: ", self$tablename, "\n", sep = "")
        cols <- paste(names(self$fields), collapse = ", ")
        cat("Columns: ", cols, "\n", sep = "")
        invisible(self)
    }

    )
)
