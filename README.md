# oRm: An Object-Relational Mapping (ORM) Framework for R

**oRm** is a lightweight ORM for R. Define models, insert data, and query relationships without writing raw SQL.

---

## 🔧 Installation

```r
remotes::install_github("kent-orr/oRm")
```

## 🚀 Quickstart

### 1. Create Engine

```r
library(oRm)

engine <- Engine$new(
  drv = RSQLite::SQLite(),
  dbname = ":memory:",
  persist = TRUE
)
```

For PostgreSQL connections, you can set a default schema that will be used
for `search_path` and by `model()` when no schema is supplied:

```r
engine <- Engine$new(
  drv = RPostgres::Postgres(),
  dbname = "mydb",
  .schema = "custom_schema"
)
```

### 2. Define Models

```r
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

### 3. Add Relationship

```r
User$define_relationship(
  local_key = "organization_id",
  type = "belongs_to",
  related_model = Organization,
  related_key = "id",
  ref = "organization",
  backref = "users"
)
```

### 4. Insert Records

```r
Organization$record(id = 1L, name = "Widgets, Inc")$create()
User$record(id = 1L, organization_id = 1L, name = "Kent", age = 34)$create()
User$record(id = 2L, organization_id = 1L, name = "Dylan", age = 25)$create()
```

### 5. Query Records

```r
kent <- User$read(id == 1, .mode = "get")
kent$data$name

org <- kent$relationship("organization")
org$data$name

org$relationship("users")  # list of user records
```

### 6. CRUD API

Verbs follow CRUD throughout: `create`/`read`/`update`/`delete` live on the
`TableModel` at the set level and reappear on `Record` for a single row — so
autocomplete on `c`, `r`, `u`, `d` gets you where you need to go on either noun.

```r
# Set-level: operate on the table
User$create(id = 5, name = "hogan")
User$update(id == 5, name = "Hogan")
User$delete(id == 5)

# Row-level: operate on a single Record
u <- User$record(id = 5, name = "hogan")$create()
u$data$name <- "Hogan"
u$update()
u$delete()
```

### 7. Read-Only Engines

Pass `.read_only = TRUE` to prevent all write operations on an engine. Useful for giving analysts a safe connection to production databases.

```r
ro_engine <- Engine$new(
  drv   = RSQLite::SQLite(),
  dbname = "prod.sqlite",
  .read_only = TRUE
)

# Reads work fine
records <- User$read()

# Any write attempt is blocked
User$record(id = 99, name = "Ghost")$create()
#> Error: Engine is read-only; refusing write operation.
```

Enforcement is applied at two levels: application-level guards on every write call, plus a dialect-specific connection-level flag (SQLite `SQLITE_RO`, PostgreSQL `default_transaction_read_only=on`, MySQL `SET SESSION TRANSACTION READ ONLY`).

### 8. Partial Models

Define a `TableModel` with only a subset of an existing table's columns. `read()` will project results to just the declared fields.

```r
# The 'users' table also has 'ssn' and 'internal_notes' columns — omit them here
UserView <- engine$model(
  "users",
  id    = Column("INTEGER", primary_key = TRUE),
  name  = Column("TEXT"),
  email = Column("TEXT")
)

UserView$read(.mode = "data.frame")
#>   id  name           email
#> 1  1  Kent  kent@example.com
```

Combine with `.read_only = TRUE` for safe, scoped access to production tables.

### 9. Reflecting Models From Existing Tables

When a table already exists, `engine$reflect()` introspects its columns and
returns a ready-to-use `TableModel`, so you can do basic CRUD without
declaring every column by hand.

```r
# Reflect all columns of the existing "users" table
Users <- engine$reflect("users")
names(Users$fields)
#> [1] "id" "name" "age" "hash"

# Keep or drop columns with include / exclude
Users <- engine$reflect("users", include = c("id", "name", "age"))
Users <- engine$reflect("users", exclude = c("hash", "configuration"))

Users$record(id = 3L, name = "Ada", age = 36L)$create()
Users$read(.mode = "data.frame")
```

The **default** reflection recovers column names and best-effort types only.
The **PostgreSQL dialect** reflects richer metadata automatically:

- Canonical types (e.g. `integer`, `text`, `timestamp with time zone`)
- Primary key flags
- Nullability and column defaults (server-side defaults like `nextval()` are
  preserved as `dbplyr::sql()` objects and applied by the database at insert
  time)
- Foreign keys, returned as `ForeignKey` objects and schema-qualified when the
  target lives in another schema

For other dialects, because the primary key is not reflected, `update()` and
`delete()` require you to supply the key column via `...`:

```r
Users <- engine$reflect("users", id = Column("INTEGER", primary_key = TRUE))
```

The `...` argument also lets you attach `Method()`s or override any reflected
column definition, exactly like `engine$model()`.

### 10. Reflecting an Entire Schema

`engine$reflect_schema()` reflects every table in a schema in one call and
automatically wires up the `many_to_one` / `one_to_many` relationships implied
by the reflected foreign keys. This is most useful with the PostgreSQL dialect,
whose reflection captures foreign keys.

```r
# Reflect all tables in the engine's default schema
models <- engine$reflect_schema()

# Or restrict to a specific set of tables
models <- engine$reflect_schema(tables = c("users", "posts", "comments"))

# Access individual models
posts  <- models$posts
users  <- models$users

# Relationships are wired automatically from FK metadata
post <- posts$read(id == 1, .mode = "get")
author <- post$relationship("users")   # many_to_one via posts.user_id -> users.id
```

Options:

| Argument | Default | Description |
|---|---|---|
| `tables` | `NULL` | Tables to reflect; `NULL` reflects all in the schema |
| `exclude` | `NULL` | Table names to skip |
| `.schema` | engine schema | Schema to inspect |
| `wire_relationships` | `TRUE` | Auto-wire FK relationships |

---

Early-stage project. Feedback welcome!
