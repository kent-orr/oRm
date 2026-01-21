# Using Methods

Methods let you attach behavior to models and records, keeping business
logic close to the data. This vignette starts with a minimal model and
adds table- and record-level helpers.

### Methods

If database records are a model where columns are attributes, you may
want reliable methods to alter, augment, or use those records. When
creating a `TableModel`, use
[`Method()`](https://kent-orr.github.io/oRm/reference/Method.md) to
define methods for your model. Methods are declared at model creation,
so each example builds a fresh model to keep it focused.

You can define methods for both the table level and the record level.
Let’s look at a table level method for students that performs a custom
search:

``` r
Student = engine$model(
    "students",
    id = Column('int'),
    name = Column('varchar'),
    age = Column('int'),

    search_by_name = Method(function(string) {
        self$read(dplyr::sql(paste0("name like '%", string, "%'")))
    }, target='table')
)
Student$create_table(overwrite = TRUE)
#> <TableModel>
#> Table: students
#> Columns: id, name, age

jane = Student$record(id=1, name = "Jane", age= 22)$create()
john = Student$record(id=2, name = "John", age = 25)$create()
tim  = Student$record(id=3, name = "Tim", age = 33)$create()

Student$search_by_name('j')
#> [[1]]
#> <Record>: 'students'
#> id: 1
#> name: Jane
#> age: 22 
#> 
#> [[2]]
#> <Record>: 'students'
#> id: 2
#> name: John
#> age: 25
```

You can also have record level methods (this is the default behavior):

``` r
Student = engine$model(
    "students",
    id = Column('int'),
    name = Column('varchar'),
    age = Column('int'),

    search_by_name = Method(function(string) {
        self$read(dplyr::sql(paste0("name like '%", string, "%'")))
    }, target='table'),

    greet = Method(function() {
        print(paste("Hi, my name is", self$data$name))
    })
)


Student$create_table(overwrite = TRUE)
#> <TableModel>
#> Table: students
#> Columns: id, name, age

jane = Student$record(id=1, name = "Jane", age= 22)$create()
jane$greet()
#> [1] "Hi, my name is Jane"
```

## Applications in Business Logic

Sometimes your model needs reusable business logic, not just CRUD.
Methods are a good place to put that logic so it stays close to the data
and is easy to test. We’ll add a `Grades` table, then give `Students` a
method to assign a grade and another to calculate an average.

First, define the grades table:

``` r
Grades = engine$model(
    "grades",
    id = Column('int', primary_key = TRUE),
    class = Column('varchar'),
    student_id = ForeignKey('int', references = 'students.id'),
    value = Column('real')
)

Grades$create_table(overwrite=TRUE)
#> <TableModel>
#> Table: grades
#> Columns: id, class, student_id, value
```

Now define a student model with three methods. `increment_id()` is a
helper that calculates the next `Grades` id (SQLite does not
auto-increment unless you declare it), `assign_grade()` creates a
related grade record using that helper, and `get_average()` summarizes a
student’s grades.

``` r
Students = engine$model(
    "students",
    id = Column('int', primary_key = TRUE),
    name = Column('varchar'),
    age = Column('int'),

    increment_id = Method(function() {
        last_grade = Grades$read(id == max(id), .mode = 'one_or_none')
        last_id = if (is.null(last_grade)) 0 else last_grade$data$id
        last_id + 1
    }),

    assign_grade = Method(function(class, value) {
        grade = Grades$record(
            id = self$increment_id(),
            class = class,
            student_id = self$data$id,
            value = value
        )$create()

        return(grade)
    }),

    get_average = Method(function() {
        grades = self$relationship('grades')
        mean(sapply(grades, \(x) x$data$value), na.rm=TRUE)
    })
)
Students$create_table(overwrite = TRUE)
#> <TableModel>
#> Table: students
#> Columns: id, name, age
```

With both tables in place, define the relationship so records can find
their grades:

``` r
define_relationship(
    Students, 'id', 'one_to_many', Grades, 'student_id',
    ref = 'grades',
    backref = 'student'
)
#> <TableModel>
#> Table: students
#> Columns: id, name, age
```

And now we can use the methods on a record:

``` r
john = Students$record(id=1, name = "john", age = 22)$create()
john$assign_grade('Math', 83)
#> Warning: Missing values are always removed in SQL aggregation functions.
#> Use `na.rm = TRUE` to silence this warning
#> This warning is displayed once every 8 hours.
#> <Record>: 'grades'
#> id: 1
#> class: Math
#> student_id: 1
#> value: 83
john$assign_grade('Math', 87)
#> <Record>: 'grades'
#> id: 2
#> class: Math
#> student_id: 1
#> value: 87
john$relationship('grades')
#> [[1]]
#> <Record>: 'grades'
#> id: 1
#> class: Math
#> student_id: 1
#> value: 83 
#> 
#> [[2]]
#> <Record>: 'grades'
#> id: 2
#> class: Math
#> student_id: 1
#> value: 87
john$get_average()
#> [1] 85
```
