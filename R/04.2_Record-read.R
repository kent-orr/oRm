#' Read records from the table using dynamic filters
#'
#' Uses a `dbplyr`-based query to read records from the associated table.
#' You can specify filter conditions and return behavior, similar to SQLAlchemy.
#'
#' @name Record$read
#' @param ... Filter expressions passed as unquoted arguments (e.g. age > 18, name == "Alice")
#' @param mode Character string: one of `"all"`, `"one_or_none"`, or `"get"`.
#'   - `"all"` returns a list of `Record` objects (default)
#'   - `"one_or_none"` returns a single `Record` or `NULL`
#'   - `"get"` returns a single `Record` and errors if more or less than one match
#'
#' @return One or more `Record` objects depending on the mode.
#' @importFrom dplyr tbl filter collect
#' @importFrom rlang sym
#' @export
Record$set("public", "read", function(..., mode = c("all", "one_or_none", "get")) {
  mode <- match.arg(mode)
  con <- self$model$get_connection()
  tbl_ref <- dplyr::tbl(con, self$model$tablename)

  # Capture expressions from ...
  filters <- rlang::enquos(...)

  # Apply filters if any
  if (length(filters) > 0) {
    tbl_ref <- dplyr::filter(tbl_ref, !!!filters)
  }

  rows <- dplyr::collect(tbl_ref)

  if (mode == "get") {
    if (nrow(rows) != 1) {
      stop("Expected exactly one row, got: ", nrow(rows))
    }
    return(Record$new(model = self$model, data = as.list(rows[1, , drop = TRUE])))
  }

  if (mode == "one_or_none") {
    if (nrow(rows) > 1) {
      stop("Expected zero or one row, but got multiple")
    } else if (nrow(rows) == 1) {
      return(Record$new(model = self$model, data = as.list(rows[1, , drop = TRUE])))
    } else {
      return(NULL)
    }
  }

  # Default: all
  lapply(seq_len(nrow(rows)), function(i) {
    Record$new(model = self$model, data = as.list(rows[i, , drop = TRUE]))
  })
})
