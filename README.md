# oRm: An Object-Relational Mapping (ORM) Framework for R

# oRm

**oRm** is a lightweight, pipe-friendly Object Relational Mapper (ORM) for R, designed for use with `DBI`, `dbplyr`, and `R6`.

Inspired by [SQLAlchemy](https://www.sqlalchemy.org/) and built with R conventions in mind, `oRm` makes it easy to define table models, insert and query records, and establish relationships between models — all without writing raw SQL.

---

## 🔧 Installation

```r
# In development: install from GitHub
remotes::install_github("kent-orr/oRm")
```

## 🚀 Quickstart

1. Set up a database `Engine`  


```{r}
library(oRm)

engine <- Engine$new(
  drv = RSQLite::SQLite(),
  dbname = ":memory:",
  persist = TRUE # note at the bottom
)

```

We used the `persist` = TRUE argument here because by default `Engine` handles opening and closing connections (or pools) for you per transaction. Normally that's handy and saves you repetitive typing, but with an SQLite table in memory we want to be sure to persist the connection so we don't dump the table between calls. 

2. Define Models and Create Tables

If you've used SQLAlchemy before this should look pretty familiar. We're going to give the table name, then list the columns. A difference here is that we will not use ForeignKey inside a column, but as a special kind of column.

```{r}
User <- engine$model(
  "users",
  id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
  organization_id = ForeignKey("INTEGER", references = "organizations.id"),
  name = Column("TEXT", nullable = FALSE),
  age = Column("INTEGER")
)

Organization <- engine$model(
  "organizations",
  id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
  name = Column("TEXT", nullable = FALSE)
)

Organization$create_table()
User$create_table()

```

3. Define Relationships (bi-directional optional)

```{r}
User |>
  define_relationship(
    local_key = "organization_id",
    type = "belongs_to",
    related_model = Organization,
    related_key = "id",
    ref = "organization",    # How to access from a User record
    backref = "users"        # How to access from an Organization record
  )
```

4. Insert Records

```{r}
Organization$record(id = 1L, name = "Widgets, Inc")$create()

User$record(id = 1L, organization_id = 1L, name = "Kent", age = 34)$create()
User$record(id = 2L, organization_id = 1L, name = "Dylan", age = 25)$create()

```

5. Query Records and Relationships  

```{r}
kent <- User$read(id == 1, mode = "get")
kent$data$name
#> [1] "Kent"

org <- kent$relationship("organization")
org$data$name
#> [1] "Widgets, Inc"

# Or access via shortcut if backref defined
org$relationship('users)
#> list of User records

```

6. Create, Read, Update, Delete Records

```{r}
# create
hogan = User$record(id=5, name = 'hogan')
hogan$create()
print(hogan)

# read
hogan = User$read(id == 5)
print(hogan)

# update
hogan$data$name = 'Hogan'
hogan$update()
print(hogan)

# delete
hogan$delete()
User$read(id == 5)


```