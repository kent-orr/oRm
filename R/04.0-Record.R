Record <- R6::R6Class(
  "Record",
  public = list(
    model = NULL,
    data = list(),
    initialize = function(model, data = list()) {
      if (!inherits(model, "BaseModel")) {
        stop("Record must be initialized with a BaseModel.")
      }
      unknown_cols <- setdiff(names(data), names(model$fields))
      if (length(unknown_cols) > 0) {
        stop("Unknown fields in data: ",
             paste(unknown_cols, collapse = ", "))
      }
      self$model <- model
      self$data <- data
    }
    )
  )
