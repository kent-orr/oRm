BaseModel <- R6::R6Class(
  "BaseModel",
  public = list(
    tablename = NULL,
    engine = NULL,
    fields = list(),

    initialize = function(tablename, engine, ...) {
      if (missing(tablename) || missing(engine)) {
        stop("Both 'tablename' and 'engine' must be provided to BaseModel.")
      }

      self$tablename <- tablename
      self$engine <- engine

      dots <- list(...)
      col_defs <-
        dots[vapply(dots, inherits, logical(1), "Column")]
      self$fields <- col_defs
    },

    get_connection = function(...) {
      self$engine$get_connection(...)
    },

    get_fields_for_dbi = function() {
      # Convert Column definitions into DBI-compatible list
      vapply(self$fields, function(col)
        col$type, FUN.VALUE = character(1))
    },

    create_table = function() {
      DBI::dbCreateTable(
        self$get_connection(),
        name = self$tablename,
        fields = self$get_fields_for_dbi()
      )
    },

    print = function(...) {
      library(crayon)

      cat("<", class(self)[1], ">\n", sep = "")
      cat("  Table: ", self$tablename, "\n", sep = "")

      if (length(self$fields) == 0) {
        cat("  Fields: (none defined)\n")
        return(invisible(self))
      }

      cat("  Fields:\n")

      field_df <- data.frame(
        name = names(self$fields),
        type = vapply(self$fields, function(x) x$type, character(1)),
        nullable = vapply(self$fields, function(x) x$nullable, logical(1)),
        key = vapply(self$fields, function(x) x$key, logical(1)),
        stringsAsFactors = FALSE
      )

      # Order key fields to top
      field_df <- field_df[order(-field_df$key), ]

      # Truncate output if needed
      n_display <- min(10, nrow(field_df))
      field_df <- field_df[seq_len(n_display), ]

      # Set fixed column widths
      col_widths <- list(
        name = max(10, max(nchar(field_df$name))),
        type = max(8, max(nchar(field_df$type))),
        null = 9
      )

      # Type coloring
      color_type <- function(x) {
        switch(tolower(x),
               "integer"   = green(x),
               "int"       = green(x),
               "numeric"   = yellow(x),
               "real"      = yellow(x),
               "text"      = blue(x),
               "varchar"   = blue(x),
               "date"      = magenta(x),
               "timestamp" = magenta(x),
               silver(x))
      }

        for (i in seq_len(nrow(field_df))) {
          row <- field_df[i, ]
          key_icon <- if (row$key) "🔑" else "  "
          name_str <- format(row$name, width = col_widths$name)

          # Step 1: format plain type to fixed width
          type_raw <- format(row$type, width = col_widths$type)
          # Step 2: apply crayon color *after* padding
          type_str <- color_type(type_raw)

          null_str <- if (row$nullable) "NULL" else "NOT NULL"

          cat(sprintf("  %s %s  %s  %s\n",
                      key_icon,
                      name_str,
                      type_str,
                      null_str))

      }

      if (length(self$fields) > n_display) {
        cat(silver(sprintf("  ... %d more columns not shown\n",
                           length(self$fields) - n_display)))
      }

      invisible(self)
    }

  )
)


