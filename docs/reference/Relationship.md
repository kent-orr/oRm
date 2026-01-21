# Relationship Class

The Relationship class represents a relationship between two models in
the ORM system.

## Public fields

- `local_model`:

  The model that owns the relationship.

- `local_key`:

  The key in the local model that relates to the foreign key in the
  related model.

- `related_model`:

  The model that is being related to.

- `related_key`:

  The key in the related model that the local_key relates to.

- `type`:

  The type of relationship (one_to_one, one_to_many, many_to_many, or
  many_to_one).

## Methods

### Public methods

- [`Relationship$new()`](#method-Relationship-new)

- [`Relationship$print()`](#method-Relationship-print)

- [`Relationship$clone()`](#method-Relationship-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Relationship object.

#### Usage

    Relationship$new(
      local_model,
      local_key,
      type = c("one_to_one", "one_to_many", "many_to_many", "many_to_one"),
      related_model,
      related_key
    )

#### Arguments

- `local_model`:

  The model that owns the relationship.

- `local_key`:

  The key in the local model that relates to the foreign key in the
  related model.

- `type`:

  The type of relationship.

- `related_model`:

  The model that is being related to.

- `related_key`:

  The key in the related model that the local_key relates to.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print a human-readable representation of the relationship.

#### Usage

    Relationship$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Relationship$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
