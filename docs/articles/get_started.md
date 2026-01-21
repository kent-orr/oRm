# Get Started

oRm is an object-relational mapping (ORM) framework designed for R users
to work with SQL. Its core building blocks follow a chain of
responsibility:

- `Engine`: Manages the database connection and model registry.
- `TableModel`: Represents a database table with column definitions and
  relationships.
- `Record`: Represents a row in a table. Supports CRUD operations.
- `Relationship`: Defines how models are linked, supporting joins and
  nested querying.
- `Method`: Attaches custom behavior to models at table or record level.

We’ll walk through each concept, starting with the `Engine`.

------------------------------------------------------------------------

## Set up the Engine

``` r
library(oRm)
```

``` r
engine <- Engine$new(
  drv = RSQLite::SQLite(),
  dbname = ":memory:",
  persist = TRUE  # Optional for in-memory databases
)
```

### What the Engine Does

- Creates and manages the DBI connection.  
- Registers models so you can reference them by name or relationship.  
- Optionally uses pool for connection pooling (set use_pool = TRUE).

You’ll rarely need to interact with the connection directly, but you
can:

``` r
engine$get_connection()
engine$list_tables()
engine$execute("SELECT * FROM users")
```

By default, connections are closed automatically after each operation
unless persist = TRUE or use_pool = TRUE.

## Define a TableModel

TableModels can be created in two ways. The first is by calling the
`TableModel` constructor directly:

``` r
Users <- TableModel$new(
  "users",
  engine,
  id = Column("INTEGER", primary_key = TRUE),
  organization_id = Column("INTEGER"),
  name = Column("TEXT"),
  age = Column('INTEGER', default = 18)
)
```

You can also set a column default using an SQL expression by wrapping it
in [`dbplyr::sql()`](https://dbplyr.tidyverse.org/reference/sql.html),
which prevents the value from being quoted as a string:

``` r
# Simple Users model without complex defaults
Users <- TableModel$new(
    "users",
    engine,
    id = Column("INTEGER", primary_key = TRUE),
    organization_id = Column("INTEGER"),
    name = Column("TEXT"),
    age = Column("INTEGER")
)
```

Or, more commonly, you define a model through the engine itself:

``` r
Organization <- engine$model(
  "organizations",
  id = Column("INTEGER", primary_key = TRUE),
  name = Column("TEXT")
)
```

This second approach automatically registers the model with the engine
for use in relationships and queries.

## What TableModels Do

A `TableModel` instance gives you access to common operations on your
table:

### Create the table in your database

``` r
Users$create_table()
#> <TableModel>
#> Table: users
#> Columns: id, organization_id, name, age
Users$record(id = 1, name='John')$create()
Users$record(id = 2, name='Jane', age = 35)$create()
```

This creates the table based on your column definitions if it doesn’t
already exist.

### Read rows from the table

``` r
all_users <- Users$read()
young_users <- Users$read(age < 30)
```

The `read()` method accepts `dbplyr`-style filter conditions through
`...`, allowing flexible querying using R expressions. It returns a list
of `Record` objects, or a single record if `.mode = "get"` is specified.

``` r
specific_user <- Users$read(id == 1, .mode = "get")
```

## What Records Do

Each row in a table is represented by a `Record`. Records provide
methods for creating, updating, deleting, and accessing individual rows.

### Create a new record

``` r
Users$record(id = 3, organization_id = 1, name = "Alice")$create()
```

### Update a record

``` r
alice <- Users$read(id == 3, .mode = "get")
alice$data$name <- "Alicia"
alice$update()
```

### Delete a record

``` r
alice$delete()
#> NULL
```

### Access record data

``` r
print(alice$data$name)
#> [1] "Alicia"
```

## Defining and Using Relationships

You can define relationships between tables to enable seamless
navigation between related records.

### Define a relationship

``` r
define_relationship(
  Users,
  local_key = "organization_id",
  type = "many_to_one",
  related_model = Organization,
  related_key = "id",
  ref = "organization",
  backref = "users"
)
#> <TableModel>
#> Table: users
#> Columns: id, organization_id, name, age
```

This allows records in `Users` to access their related `Organization`,
and records in `Organization` to access all related `Users`.

### Accessing relationships through a Record

We defined the Organization earlier, but the table itself was nevver
created. Let’s create our table and give it an Organization to work
with.

``` r
Organization$create_table()
#> <TableModel>
#> Table: organizations
#> Columns: id, name
Organization$record(id = 1, name = "Widgets, Inc")$create()
```

``` r

Users$record(id = 3, name = 'Alice', organization_id = 1)$create()
alice = Users$read(id == 3, .mode='get')
alice_org <- alice$relationship('organization')
print(alice_org$data$name)
#> [1] "Widgets, Inc"
```

### Accessing relationships through a TableModel

``` r
young_orgs <- Organization$relationship("users", age < 30)
young_orgs
#> list()
```

This returns a list of user records with `age < 30` that belong to each
organization.

## Defining and Using Methods

Methods let you attach custom behavior to your models, keeping business
logic close to your data. You can define methods at both the table level
and the record level using the
[`Method()`](https://kent-orr.github.io/oRm/reference/Method.md)
function.

### Table-level methods

Table-level methods operate on the entire table and are useful for
custom queries or bulk operations:

``` r
Users <- engine$model(
  "users",
  id = Column("INTEGER", primary_key = TRUE),
  organization_id = Column("INTEGER"),
  name = Column("TEXT"),
  age = Column("INTEGER"),

  search_by_name = Method(function(string) {
    self$read(dplyr::sql(paste0("name like '%", string, "%'")))
  }, target = 'table')
)

Users$create_table(overwrite = TRUE)
#> <TableModel>
#> Table: users
#> Columns: id, organization_id, name, age
Users$record(id = 1, name = "John", age = 25)$create()
Users$record(id = 2, name = "Jane", age = 30)$create()

# Use the custom table method
Users$search_by_name('J')
#> [[1]]
#> <Record>: 'users'
#> id: 1
#> organization_id: NA
#> name: John
#> age: 25 
#> 
#> [[2]]
#> <Record>: 'users'
#> id: 2
#> organization_id: NA
#> name: Jane
#> age: 30
```

### Record-level methods

Record-level methods operate on individual records and are useful for
instance-specific operations:

``` r
Users <- engine$model(
  "users",
  id = Column("INTEGER", primary_key = TRUE),
  name = Column("TEXT"),
  age = Column("INTEGER"),

  greet = Method(function() {
    print(paste("Hi, my name is", self$data$name))
  })
)

Users$create_table(overwrite = TRUE)
#> <TableModel>
#> Table: users
#> Columns: id, name, age
jane <- Users$record(id = 1, name = "Jane", age = 30)$create()

# Use the custom record method
jane$greet()
#> [1] "Hi, my name is Jane"
```

For more details on using methods to implement business logic, see the
[Using
Methods](https://kent-orr.github.io/oRm/articles/using-methods.md)
vignette.
