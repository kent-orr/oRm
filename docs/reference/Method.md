# Define a method to attach to a model

Define a method to attach to a model

## Usage

``` r
Method(fn, target = c("record", "table"))
```

## Arguments

- fn:

  A function containing the method implementation

- target:

  Character. Either "record" for instance methods or "table" for
  model-level methods

## Value

An orm_method object

## Details

The \`Method()\` function provides explicit attachment of custom methods
to ORM models. Methods defined with \`target = "record"\` become
instance methods available on individual Record objects and have access
to \`self\` (the record instance). Methods defined with \`target =
"table"\` become class methods available on the TableModel itself.

Within method functions, \`self\` refers to either the Record (for
"record" target) or TableModel (for "table" target), following R6
conventions.

## Examples

``` r
# Define a Students model with custom methods
if (FALSE) { # \dontrun{
Students <- engine$model(
  "students",
  id = Column("INTEGER", primary_key = TRUE),
  name = Column("TEXT"),
  present = Column("INTEGER", default = 0),

  # Instance method - available on individual student records
  greet = Method(function() {
    paste("Hello, I'm", self$data$name)
  }, target = "record"),

  # Instance method - modify record state
  mark_present = Method(function() {
    self$present <- 1
    self$save()
  }, target = "record"),

  # Class method - available on the Students model
  find_by_name = Method(function(name) {
    self$read(name = name)
  }, target = "table")
)

# Using the methods:
student <- Students$read_one(id = 1)
student$greet()  # "Hello, I'm Alice"
student$mark_present()  # Updates and saves the record

Students$find_by_name("Bob")  # Returns matching records
} # }
```
