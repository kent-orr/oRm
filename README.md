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
User |> define_relationship(
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

```r
u <- User$record(id = 5, name = "hogan")
u$create()
u <- User$read(id == 5)
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

---

Early-stage project. Feedback welcome!
