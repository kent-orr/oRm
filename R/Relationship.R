Relationship = R6::R6Class(
  'Relationship',
  public = list(
    local_model = NULL,
    local_key = NULL,
    related_model = NULL,
    related_key = NULL,
    type = NULL,

    initialize = function(
      local_model, 
      local_key,
      type = c('belongs_to', 'owns', 'one_to_one', 'one_to_many', 'many_to_many', 'many_to_one'),
      related_model,
      related_key
  ) {
      self$local_model <- local_model
      self$local_key <- local_key
      self$related_model <- related_model
      self$related_key <- related_key
    },

    print = function() {
      cat(sprintf("Relationship: '%s.%s' => '%s.%s'", 
        self$local_model$tablename, self$local_key,
        self$related_model$tablename, self$related_key
      ))
    }
  )
)

define_relationship = function(
  local_model, 
  local_key, 
  type = c('one_to_one', 'one_to_many', 'many_to_one', 'many_to_many'), 
  related_model, 
  related_key, ref = NULL, backref = NULL) {
  if (is.null(ref)) {
    ref <- tolower(related_model$tablename)
  }

  relationship = Relationship$new(local_model, local_key, type, related_model, related_key)
  local_model$relationships[[ref]] = relationship

  if (!isFALSE(backref)) {
    if (is.null(backref)) {
      backref <- tolower(local_model$tablename)
    }
    backref_relationship = Relationship$new(related_model, related_key, type, local_model, local_key)
    related_model$relationships[[backref]] = backref_relationship
  }

  return(local_model)

}