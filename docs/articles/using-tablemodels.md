# Using TableModels

``` r
library(oRm)

engine <- Engine$new(
  drv = RSQLite::SQLite(),
  dbname = ":memory:",
  persist = TRUE
)
```

TableModels define the structure of your database tables. While you
don’t need to be creating a database from scratch to define a
TableModel, be aware: settings like `on_delete` in a column definition
won’t work if the table already exists without that constraint.

In other words, especially when working with existing databases, it’s
important to keep your model in sync with the actual database schema.

## Creating Tables

You can define a TableModel with `TableModel$new()`, but the more common
(and convenient) way is through the Engine using `engine$model()`:

``` r
Classes <- engine$model(
    tablename = "classes", 
    id = Column('INTEGER', primary_key = TRUE),
    subject = Column('TEXT'),
    teacher_id = ForeignKey('INTEGER', references = 'teachers.id'),
    grade_average = Column('NUMBER', default = \(x) rnorm(1, 80, 10))
)
Classes$create_table()
#> <TableModel>
#> Table: classes
#> Columns: id, subject, teacher_id, grade_average
```

- `tablename` becomes the actual table name in the database.
- Named arguments define each column as either a `Column` or
  `ForeignKey`.

### Columns

[`Column()`](https://kent-orr.github.io/oRm/reference/Column.md) and
[`ForeignKey()`](https://kent-orr.github.io/oRm/reference/ForeignKey.md)
are S3 constructors used to define field metadata.

- The database dialect (e.g., SQLite, Postgres) affects how the model is
  translated into SQL.  
- Default values can be literal values or functions. If it’s a function,
  it will be evaluated when a record is created and no value is
  provided.
- [`ForeignKey()`](https://kent-orr.github.io/oRm/reference/ForeignKey.md)
  is a special kind of
  [`Column()`](https://kent-orr.github.io/oRm/reference/Column.md) that
  links to another table, using the format “other_table.column_name”.  
- You can also pass raw SQL strings or named options via … to inject
  additional behavior into the CREATE TABLE statement.

When you supply a default value, it can be either a character string or
a function. If a string, it’s used as the default value. If a function,
it’s called with no arguments and the result is used as the default
value. that R function gets called by the TableModel when creating a
record with no value provided.

`ForeignKey` is a special case of Column that specifies a foreign key
relationship to another table. Typically you provide the target as a
single string using the `references = "other_table.column"` syntax,
though `ref_table` and `ref_column` can also be supplied separately if
needed.

## Reading Data

The R in C**R**UD happens at the TableModel level. Under the hood, the
read method is making use of `dbplyr` and the dot args given to the read
method are directly supplied to a
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
call. This allows you to use familiar `dplyr` syntax to retrieve the
records you want to work with. We’re going to create a few entries in
our table, skipping over that section right now. If you want to skip
ahead and come back, you can jump to the [using
records](https://kent-orr.github.io/using-records.md) section.

``` r
# Let's make some classes
for (i in 1:10) {
    Classes$record(
        id = i, 
        subject = ifelse(i %% 2 == 0, "Math", "Science"),
        teacher_id = ifelse(i %% 2 == 0, 1, 2)
    )$create()
}

# Now let's look at some classes
# calling with no args returns all records
classes = Classes$read()
print(length(classes))
#> [1] 10

# calling with a filter argument returns records matching the filter
classes = Classes$read(subject == "Math")
print(length(classes))
#> [1] 5

Classes$read(id == 2, .mode='get')
#> <Record>: 'classes'
#> grade_average: 82.5531705484526
#> id: 2
#> subject: Math
#> teacher_id: 1
```

### Modes

There are five modes for reading data: ‘get’, ‘one_or_none’, ‘all’,
‘data.frame’, and ‘tbl’.

- ‘get’ will return a single record that should be matched by UID. If no
  matching record is found, it will throw an error.
- ‘one_or_none’ will return a single record. If no matching record is
  found, it will return NULL instead of throwing an error.
- ‘all’ will return all records that match the filter.

The big difference between ‘get’ and the other two is that get will
return the object itself, while the other two will always return a list
if not NULL. This allows your code to enforce data integrity when you’re
trying to get a single record, and be the same whether your filter
returns one or multiple records.

### Sorting Returned Records

Ordering your returned records can be achieved with the .order_by
argument. It’s a list of unquoted column names that will get applied to
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html).
So if it works in `arrange` call, it should work here as well, and that
includes the designation of `desc()` to sort in descending order.

``` r
Classes$read(.order_by = c(subject, desc(id))) |>
    sapply(function(x) {paste(x$data$subject, x$data$id)}) |>
    suppressWarnings() # sqlite is noisy about arrange and limits
#>  [1] "Math 10"   "Math 8"    "Math 6"    "Math 4"    "Math 2"    "Science 9"
#>  [7] "Science 7" "Science 5" "Science 3" "Science 1"
```

### Offsets and Limits

The read method also accepts an offset and a limit argument. These
arguments are used to control the pagination of the results. Unlike a
dataframe, the printing of a list of records will not truncate itself
neatly, and with no filter all of the records will return. There is a
default limt of 100 records to ensure that you don’t get too much data
at once. You can set this to NULL to override.

Used in conjunction, you can effectivly paginate your records.

### Table-Level Methods

You can extend your TableModel with custom behavior using the
[`Method()`](https://kent-orr.github.io/oRm/reference/Method.md)
function. Table-level methods operate on the entire table and are useful
for custom queries, bulk operations, or complex business logic:

``` r
Classes <- engine$model(
    tablename = "classes",
    id = Column('INTEGER', primary_key = TRUE),
    subject = Column('TEXT'),
    teacher_id = ForeignKey('INTEGER', references = 'teachers.id'),
    grade_average = Column('NUMBER', default = \(x) rnorm(1, 80, 10)),

    # Table-level method
    get_by_subject = Method(function(subject_name) {
        self$read(subject == subject_name)
    }, target = 'table'),

    # Another table-level method
    get_class_stats = Method(function() {
        all_classes <- self$read()
        list(
            total = length(all_classes),
            avg_grade = mean(sapply(all_classes, \(x) x$data$grade_average))
        )
    }, target = 'table')
)

Classes$create_table(overwrite = TRUE)
#> <TableModel>
#> Table: classes
#> Columns: id, subject, teacher_id, grade_average

# Create some sample data
for (i in 1:10) {
    Classes$record(
        id = i,
        subject = ifelse(i %% 2 == 0, "Math", "Science"),
        teacher_id = ifelse(i %% 2 == 0, 1, 2)
    )$create()
}

# Use custom table methods
math_classes <- Classes$get_by_subject("Math")
print(length(math_classes))
#> [1] 5

stats <- Classes$get_class_stats()
print(stats)
#> $total
#> [1] 10
#> 
#> $avg_grade
#> [1] 80.89422
```

Table methods have access to `self`, which refers to the TableModel
instance. This allows you to call any TableModel method like
`self$read()`, `self$record()`, or `self$relationship()`.

For more examples of using methods to implement business logic, see the
[Using
Methods](https://kent-orr.github.io/oRm/articles/using-methods.md)
vignette.
