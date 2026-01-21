# Define a relationship between two models

This function establishes a relationship between two models in the ORM
system. It creates a Relationship object and adds it to the local
model's relationships. If a backref is specified, it also creates a
reverse relationship in the related model. As this uses R6 classes, the
models are modified in place and do not need to be reassigned.

## Usage

``` r
define_relationship(
  local_model,
  local_key,
  type = c("one_to_one", "one_to_many", "many_to_one", "many_to_many"),
  related_model,
  related_key,
  ref = NULL,
  backref = NULL
)
```

## Arguments

- local_model:

  The model that owns the relationship.

- local_key:

  The key in the local model that relates to the foreign key in the
  related model.

- type:

  The type of relationship. Must be one of 'one_to_one', 'one_to_many',
  'many_to_one', or 'many_to_many'.

- related_model:

  The model that is being related to.

- related_key:

  The key in the related model that the local_key relates to.

- ref:

  The name to use for this relationship in the local model. If NULL,
  defaults to the lowercase name of the related model.

- backref:

  The name to use for the reverse relationship in the related model. If
  NULL, defaults to the lowercase name of the local model. If FALSE, no
  reverse relationship is created.

## Value

Invisibly returns the local_model (although the model is modified in
place).

## Examples

``` r
# \donttest{
# Set up models
User <- engine$model("users", id = Column("INTEGER", primary_key = TRUE))
#> Error: object 'engine' not found
Post <- engine$model("posts", 
                     id = Column("INTEGER", primary_key = TRUE), 
                     user_id = Column("INTEGER"))
#> Error: object 'engine' not found

# Define a one-to-many relationship from User to Post
define_relationship(User, "id", "one_to_many", Post, "user_id", 
                    ref = "posts", backref = "user")
#> Error: object 'User' not found

# Now User has a 'posts' relationship and Post has a 'user' relationship

# Create some sample data
user <- User$record(id = 1)$create()
#> Error: object 'User' not found
post <- Post$record(id = 1, user_id = 1)$create()
#> Error: object 'Post' not found

# Use the relationships
user_posts <- user$relationship('posts')
#> Error: object 'user' not found
post_user <- post$relationship('user')
#> Error: object 'post' not found

print(user_posts)  # Should show the post
#> Error: object 'user_posts' not found
print(post_user)   # Should show the user
#> Error: object 'post_user' not found
# }
```
