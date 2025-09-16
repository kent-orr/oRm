#' Relationship Class
#'
#' @description
#' The Relationship class represents a relationship between two models in the ORM system.
#'
#' @field local_model The model that owns the relationship.
#' @field local_key The key in the local model that relates to the foreign key in the related model.
#' @field related_model The model that is being related to.
#' @field related_key The key in the related model that the local_key relates to.
#' @field type The type of relationship (one_to_one, one_to_many, many_to_many, or many_to_one).
#'
#' @export
Relationship = R6::R6Class(
  'Relationship',
  public = list(
    local_model = NULL,
    local_key = NULL,
    related_model = NULL,
    related_key = NULL,
    type = NULL,

  #' @description
  #' Create a new Relationship object.
  #' @param local_model The model that owns the relationship.
  #' @param local_key The key in the local model that relates to the foreign key in the related model.
  #' @param type The type of relationship.
  #' @param related_model The model that is being related to.
  #' @param related_key The key in the related model that the local_key relates to.
  initialize = function(
    local_model, 
    local_key,
    type = c('one_to_one', 'one_to_many', 'many_to_many', 'many_to_one'),
    related_model,
    related_key
  ) {
      self$local_model <- local_model
      self$local_key <- local_key
      self$type = match.arg(type)
      self$related_model <- related_model
      self$related_key <- related_key
    },

    #' @description
    #' Print a human-readable representation of the relationship.
    print = function() {
      lhs = strsplit(self$type, '_to_')[[1]][1]
      rhs = strsplit(self$type, '_to_')[[1]][2]
      cat(
        sprintf(
          "Relationship: %s '%s.%s' => %s '%s.%s'", 
          lhs,
          self$local_model$tablename, self$local_key,
          rhs, 
          self$related_model$tablename, self$related_key
      ))
    }
  )
)

#' Define a relationship between two models
#'
#' This function establishes a relationship between two models in the ORM system.
#' It creates a Relationship object and adds it to the local model's relationships.
#' If a backref is specified, it also creates a reverse relationship in the related model.
#' As this uses R6 classes, the models are modified in place and do not need to be reassigned.
#'
#' @param local_model The model that owns the relationship.
#' @param local_key The key in the local model that relates to the foreign key in the related model.
#' @param type The type of relationship. Must be one of 'one_to_one', 'one_to_many', 'many_to_one', or 'many_to_many'.
#' @param related_model The model that is being related to.
#' @param related_key The key in the related model that the local_key relates to.
#' @param ref The name to use for this relationship in the local model. If NULL, defaults to the lowercase name of the related model.
#' @param backref The name to use for the reverse relationship in the related model. If NULL, defaults to the lowercase name of the local model. If FALSE, no reverse relationship is created.
#'
#' @return Invisibly returns the local_model (although the model is modified in place).
#'
#' @examples
#' \donttest{
#' # Set up models
#' User <- engine$model("users", id = Column("INTEGER", primary_key = TRUE))
#' Post <- engine$model("posts", 
#'                      id = Column("INTEGER", primary_key = TRUE), 
#'                      user_id = Column("INTEGER"))
#' 
#' # Define a one-to-many relationship from User to Post
#' define_relationship(User, "id", "one_to_many", Post, "user_id", 
#'                     ref = "posts", backref = "user")
#' 
#' # Now User has a 'posts' relationship and Post has a 'user' relationship
#' 
#' # Create some sample data
#' user <- User$record(id = 1)$create()
#' post <- Post$record(id = 1, user_id = 1)$create()
#' 
#' # Use the relationships
#' user_posts <- user$relationship('posts')
#' post_user <- post$relationship('user')
#' 
#' print(user_posts)  # Should show the post
#' print(post_user)   # Should show the user
#' }
#'
#' @export
define_relationship = function(
  local_model,
  local_key,
  type = c('one_to_one', 'one_to_many', 'many_to_one', 'many_to_many'),
  related_model,
  related_key,
  ref = NULL,
  backref = NULL) {

  if (is.null(ref)) {
    ref <- tolower(related_model$tablename)
  }

  if (!(local_key %in% names(local_model$fields))) {
    stop(sprintf("Field '%s' not found in model '%s'", local_key, local_model$tablename), call. = FALSE)
  }
  if (!(related_key %in% names(related_model$fields))) {
    stop(sprintf("Field '%s' not found in model '%s'", related_key, related_model$tablename), call. = FALSE)
  }

  fk_field <- local_model$fields[[local_key]]
  if (inherits(fk_field, "ForeignKey")) {
    expected <- paste0(related_model$tablename, ".", related_key)
    if (is.null(fk_field$references) || fk_field$references != expected) {
      stop(sprintf("Field '%s' must reference '%s'", local_key, expected), call. = FALSE)
    }
  }

  relationship = Relationship$new(local_model, local_key, type, related_model, related_key)
  local_model$relationships[[ref]] = relationship

  if (!isFALSE(backref)) {
    if (is.null(backref)) {
      backref <- tolower(local_model$tablename)
    }

    reverse_type <- switch(type,
      "one_to_one" = "one_to_one",
      "one_to_many" = "many_to_one",
      "many_to_one" = "one_to_many",
      "many_to_many" = "many_to_many"
    )

    backref_relationship = Relationship$new(related_model, related_key, reverse_type, local_model, local_key)
    related_model$relationships[[backref]] = backref_relationship
  }

  return(local_model)

}