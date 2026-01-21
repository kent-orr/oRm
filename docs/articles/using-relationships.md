# Using Relationships

Having relationships between tables in a relational database can
significantly enhance data modeling and querying capabilities. But it
can also lead to complex queries and difficulty maintaining data
integrity.

In `oRm`, relationships aren’t full R6 objects like `Engine`,
`TableModel`, or `Record`, but they are attributes of TableModels that
get inherited by their Records. This makes it possible to traverse
linked tables using the record you’re working with.

------------------------------------------------------------------------

### Getting Started

Let’s create `Students` and `Classes` tables and populate them with some
dummy data.

``` r
library(oRm)
engine <- Engine$new(
  drv = RSQLite::SQLite(),
  dbname = ":memory:",
  persist = TRUE
)
Classes <- engine$model(
    tablename = "classes", 
    id = Column('INTEGER', primary_key = TRUE),
    subject = Column('TEXT'),
    teacher_id = ForeignKey('INTEGER', references = 'teachers.id')
)
Classes$create_table(overwrite=T)
#> <TableModel>
#> Table: classes
#> Columns: id, subject, teacher_id

set.seed(100)
Students <- engine$model(
    tablename = "students", 
    id = Column('INTEGER', primary_key = TRUE),
    name = Column('TEXT'),
    class_id = ForeignKey('INTEGER', references = 'classes.id'),
    grade = Column('NUMBER', default = \(x) round(rnorm(1, 85, 5)))
)
Students$create_table(overwrite=T)
#> <TableModel>
#> Table: students
#> Columns: id, name, class_id, grade


for (i in 1:10) {
    Classes$record(
        id = i, 
        subject = ifelse(i %% 2 == 0, "Math", "Science"),
        teacher_id = ifelse(i %% 2 == 0, 1, 2)
    )$create()
}
Classes$read(.limit = 3)
#> [[1]]
#> <Record>: 'classes'
#> id: 1
#> subject: Science
#> teacher_id: 2 
#> 
#> [[2]]
#> <Record>: 'classes'
#> id: 2
#> subject: Math
#> teacher_id: 1 
#> 
#> [[3]]
#> <Record>: 'classes'
#> id: 3
#> subject: Science
#> teacher_id: 2

student_names <- c("Alice", "Bob", "Charlie", "Diana", "Eve", "Frank", "Grace", "Henry", "Ivy", "Jack")
for (i in 1:100) {
    Students$record(
        id = i, 
        name = paste(sample(student_names, 1), i),
        class_id = sample(1:10, 1)
    )$create()
}
Students$read(.limit = 3)
#> [[1]]
#> <Record>: 'students'
#> grade: 86
#> id: 1
#> name: Jack 1
#> class_id: 7 
#> 
#> [[2]]
#> <Record>: 'students'
#> grade: 89
#> id: 2
#> name: Charlie 2
#> class_id: 9 
#> 
#> [[3]]
#> <Record>: 'students'
#> grade: 82
#> id: 3
#> name: Grace 3
#> class_id: 6
```

Okay, so in the old days, before `oRm` you might use dplyr to view
related fields between tables via joins. We’ll look at all the students
in class with id=1

``` r
class_tbl <- Classes$tbl()
student_tbl <- Students$tbl()

dplyr::left_join(class_tbl, student_tbl, by = c(id = "class_id")) |>
    dplyr::filter(id == 1) |>
    dplyr::collect()
#> # A tibble: 7 × 6
#>      id subject teacher_id  id.y name       grade
#>   <int> <chr>        <int> <int> <chr>      <int>
#> 1     1 Science          2    11 Grace 11      82
#> 2     1 Science          2    30 Henry 30      86
#> 3     1 Science          2    41 Bob 41        82
#> 4     1 Science          2    61 Grace 61      84
#> 5     1 Science          2    64 Charlie 64    92
#> 6     1 Science          2    81 Charlie 81    87
#> 7     1 Science          2    89 Diana 89      83
```

That is a beutiful join that finds all students in class 1. Ready for
all kinds of analysis now. But what if you’re not trying to read and
analyze the data, but make a change? What if we need to apply a curve to
the grades in class 1? We can splice things `dplyr` style and hope to
get our joins right, or, we could rely on a previously described
relationship to get the necessary records and apply the curve.

``` r
define_relationship(
    local_model = Classes, 
    local_key = 'id',
    type = 'one_to_many',
    related_model = Students, 
    related_key = 'class_id',
    ref = 'students',
    backref = 'class'
)
#> <TableModel>
#> Table: classes
#> Columns: id, subject, teacher_id
```

The possible types are ‘one_to_many’, ‘one_to_one’, ‘many_to_many’, or
‘many_to_one’. The ref and backref arguments are used to define the
relationship between the two models. You’ll use those values to call on
related records. Let’s see that in action:

``` r
class1 = Classes$read(id == 1, .mode='get')
class1_students = class1$relationship('students')
class1_students |> sapply(\(x) paste(
    x$data$name, round(x$data$grade), sep = ': '))
#> [1] "Grace 11: 82"   "Henry 30: 86"   "Bob 41: 82"     "Grace 61: 84"  
#> [5] "Charlie 64: 92" "Charlie 81: 87" "Diana 89: 83"

# let's go ahead and apply that curve to the grades
for (student in class1_students) {
    student$data$grade <- student$data$grade + 3
    student$update()
}

class1_students |> sapply(\(x) paste(
    x$data$name, round(x$data$grade), sep = ': '))
#> [1] "Grace 11: 85"   "Henry 30: 89"   "Bob 41: 85"     "Grace 61: 87"  
#> [5] "Charlie 64: 95" "Charlie 81: 90" "Diana 89: 86"
```

And if we look at that in reverse:

``` r
class1_students[[1]]$relationship('class')
#> <Record>: 'classes'
#> id: 1
#> subject: Science
#> teacher_id: 2
```

We can also apply filtering to the related records:

``` r
class1$relationship('students', grade < 87)
#> [[1]]
#> <Record>: 'students'
#> grade: 85
#> id: 11
#> name: Grace 11
#> class_id: 1 
#> 
#> [[2]]
#> <Record>: 'students'
#> grade: 85
#> id: 41
#> name: Bob 41
#> class_id: 1 
#> 
#> [[3]]
#> <Record>: 'students'
#> grade: 86
#> id: 89
#> name: Diana 89
#> class_id: 1
```

## Nested Relationships

Let’s expand on the complexity by creating a new table for teachers.
There are only a handful of teachers, and they may have multipe classes
to take care of, so we’ll assign teachers to multiple classes.

``` r
set.seed(100)
Teachers <- engine$model(
    tablename = "teachers", 
    id = Column('INTEGER', primary_key = TRUE),
    name = Column('TEXT')
)
Teachers$create_table(overwrite=T)
#> <TableModel>
#> Table: teachers
#> Columns: id, name

for (i in 1:3) {
    Teachers$record(id = i)$create()
}
Teachers$read()[[1]]
#> <Record>: 'teachers'
#> id: 1
#> name: NA


set.seed(100)
TeacherAssignments <- engine$model(
    tablename = "teacher_assignments", 
    id = Column('INTEGER', primary_key = TRUE),
    teacher_id = ForeignKey('INTEGER', references = 'teachers.id'),
    class_id = ForeignKey('INTEGER', references = 'classes.id')
)

TeacherAssignments$create_table(overwrite=T)
#> <TableModel>
#> Table: teacher_assignments
#> Columns: id, teacher_id, class_id

for (i in 1:length(Classes$read())) {
    TeacherAssignments$record(
        teacher_id = sample(1:3, 1),
        class_id = i
    )$create()
} 
TeacherAssignments$read()[1]
#> [[1]]
#> <Record>: 'teacher_assignments'
#> id: 1
#> teacher_id: 2
#> class_id: 1
```

We have two relationships we need to deine now: ‘one_to_many’ from
Teachers to TeacherAssignments, and ‘many_to_one’ from
TeacherAssignments to Classes.

``` r
define_relationship(
    local_model = Teachers, 
    local_key = 'id',
    type = 'one_to_many',
    related_model = TeacherAssignments,
    related_key = 'teacher_id',
    ref = 'teacher_assignments',
    backref = 'teacher'
)
#> <TableModel>
#> Table: teachers
#> Columns: id, name

define_relationship(
    local_model = TeacherAssignments, 
    local_key = 'class_id',
    type ='one_to_one',
    related_model = Classes,
    related_key = 'id',
    ref = 'class',
    backref = 'teacher_assignment'
)
#> <TableModel>
#> Table: teacher_assignments
#> Columns: id, teacher_id, class_id
```

You can now traverse from `Teachers` -\> `TeacherAssignments` -\>
`Classes` and back again. TO demonstrate that, we’ll make a deck of info
cards for a teacher showing each class and the students in each class.

``` r
teacher <- Teachers$read(id == 3, .mode='get')

# teacher$relationship('teacher_assignments') |>
#     lapply(\(x) x$relationship('class')) |>
#     lapply(\(x) x$relationship('students'))

bslib::card(
    bslib::card_title(teacher$data$name),
    bslib::card_body(
        teacher$relationship('teacher_assignments') |>
            lapply(\(x) {
                class = x$relationship('class')
                bslib::card(max_height = '800px',
                    bslib::card_title(class$data$subject),
                    bslib::card_body(
                        class$relationship('students') |>
                            lapply(\(x) htmltools::tags$p(paste(x$data$name, round(x$data$grade))))
                    
                    )
                )
            })
    )
)
```

##### NA

##### Math

Diana 10 89

Alice 37 79

Diana 40 78

Diana 45 90

Grace 46 88

Bob 65 82

Grace 71 94

Henry 74 87

Alice 79 80

Alice 87 82

##### Math

Diana 8 88

Charlie 33 84

Jack 44 78

Charlie 51 89

Diana 56 88

Alice 57 79

Jack 75 83

##### Math

Frank 13 84

Bob 27 87

Ivy 72 84

Eve 80 80

Bob 82 78

Bob 92 89

Diana 98 84
