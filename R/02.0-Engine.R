Engine <- R6::R6Class(
  "Engine",
  public = list(
    conn_args = NULL,
    conn = NULL,

    initialize = function(..., conn_args = list()) {
      dots <- list(...)
      if (length(dots) > 0) {
        self$conn_args <- dots
      } else {
        self$conn_args <- conn_args
      }
    },

    get_connection = function(...) {
      if (is.null(self$conn) || !DBI::dbIsValid(self$conn)) {
        args <- modifyList(self$conn_args, list(...))
        self$conn <-
          do.call(DBI::dbConnect, args)
      }
      self$conn
    },

    close = function() {
      if (!is.null(self$conn) && DBI::dbIsValid(self$conn)) {
        DBI::dbDisconnect(self$conn)
        self$conn <- NULL
      }
    },

    list_tables = function() {
      con <- self$get_connection()
      DBI::dbListTables(con)
    },

    get_query = function(sql, ...) {
      con <- self$get_connection(...)
      DBI::dbGetQuery(con, sql)
    },

    execute = function(sql, ...) {
      con <- self$get_connection(...)
      DBI::dbExecute(con, sql)
    },

    model = function(tablename, ...) {
      BaseModel$new(tablename = tablename, engine = self, ...)
    }
  )
)
