# Tests for defining a TableModel that declares only a subset of an
# existing table's columns. The use case is safe, read-only / exploratory
# navigation of an extant (e.g. production) table where the user only
# wants to surface the columns relevant to their analysis and traverse
# relationships between them.

make_engine <- function() {
  Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )
}

# Seed a "production-like" schema directly via SQL so the partial models
# below are unambiguously decoupled from the table's full definition.
seed_prod_schema <- function(engine) {
  con <- engine$get_connection()
  DBI::dbExecute(con, "
    CREATE TABLE users (
      id          INTEGER PRIMARY KEY,
      name        TEXT NOT NULL,
      email       TEXT NOT NULL,
      ssn         TEXT,
      internal_notes TEXT,
      created_at  TIMESTAMP,
      updated_at  TIMESTAMP
    )
  ")
  DBI::dbExecute(con, "
    CREATE TABLE orders (
      id          INTEGER PRIMARY KEY,
      user_id     INTEGER NOT NULL,
      total_cents INTEGER NOT NULL,
      payment_token TEXT,
      created_at  TIMESTAMP
    )
  ")
  DBI::dbExecute(con, "
    INSERT INTO users (id, name, email, ssn, internal_notes, created_at)
    VALUES
      (1, 'Alice',   'alice@example.com',   '111-11-1111', 'vip',     '2026-01-01'),
      (2, 'Bob',     'bob@example.com',     '222-22-2222', 'new',     '2026-01-02'),
      (3, 'Charlie', 'charlie@example.com', '333-33-3333', 'flagged', '2026-01-03')
  ")
  DBI::dbExecute(con, "
    INSERT INTO orders (id, user_id, total_cents, payment_token, created_at)
    VALUES
      (10, 1, 5000, 'tok_a1', '2026-02-01'),
      (11, 1, 2500, 'tok_a2', '2026-02-02'),
      (12, 2, 9999, 'tok_b1', '2026-02-03')
  ")
}

test_that("read() returns Records when the model declares a subset of columns", {
  engine <- make_engine()
  on.exit(engine$close(), add = TRUE)
  seed_prod_schema(engine)

  # Partial model: omits ssn, internal_notes, updated_at.
  UserView <- TableModel$new(
    tablename = "users",
    engine = engine,
    id    = Column("INTEGER", primary_key = TRUE),
    name  = Column("TEXT"),
    email = Column("TEXT")
  )

  rows <- UserView$all()
  expect_length(rows, 3)
  expect_true(all(vapply(rows, inherits, logical(1), "Record")))

  # Records expose only the declared columns.
  expect_setequal(names(rows[[1]]$data), c("id", "name", "email"))
  expect_false("ssn" %in% names(rows[[1]]$data))
  expect_false("internal_notes" %in% names(rows[[1]]$data))

  alice <- UserView$get(id == 1)
  expect_equal(alice$data$name, "Alice")
  expect_equal(alice$data$email, "alice@example.com")
  expect_null(alice$data$ssn)

  none <- UserView$one_or_none(id == 999)
  expect_null(none)
})

test_that("data.frame and tbl modes only surface declared columns on a partial model", {
  engine <- make_engine()
  on.exit(engine$close(), add = TRUE)
  seed_prod_schema(engine)

  UserView <- TableModel$new(
    tablename = "users",
    engine = engine,
    id    = Column("INTEGER", primary_key = TRUE),
    email = Column("TEXT")
  )

  df <- UserView$read(.mode = "data.frame")
  expect_s3_class(df, "data.frame")
  expect_setequal(names(df), c("id", "email"))
  expect_equal(nrow(df), 3)

  collected <- UserView$read(.mode = "tbl") |> dplyr::collect()
  expect_setequal(names(collected), c("id", "email"))
})

test_that("filtering on a declared column works on a partial model", {
  engine <- make_engine()
  on.exit(engine$close(), add = TRUE)
  seed_prod_schema(engine)

  UserView <- TableModel$new(
    tablename = "users",
    engine = engine,
    id    = Column("INTEGER", primary_key = TRUE),
    name  = Column("TEXT"),
    email = Column("TEXT")
  )

  matches <- UserView$all(name == "Bob")
  expect_length(matches, 1)
  expect_equal(matches[[1]]$data$id, 2)
})

test_that("relationships traverse between two partial models over existing tables", {
  engine <- make_engine()
  on.exit(engine$close(), add = TRUE)
  seed_prod_schema(engine)

  UserView <- TableModel$new(
    tablename = "users",
    engine = engine,
    id    = Column("INTEGER", primary_key = TRUE),
    name  = Column("TEXT")
  )

  # Partial orders model: omits payment_token (sensitive) and created_at.
  OrderView <- TableModel$new(
    tablename = "orders",
    engine = engine,
    id          = Column("INTEGER", primary_key = TRUE),
    user_id     = Column("INTEGER"),
    total_cents = Column("INTEGER")
  )

  define_relationship(
    local_model = UserView,
    local_key = "id",
    type = "one_to_many",
    related_model = OrderView,
    related_key = "user_id",
    ref = "orders",
    backref = "user"
  )

  alice <- UserView$get(id == 1)
  alice_orders <- alice$relationship("orders")
  expect_length(alice_orders, 2)
  alice_order_ids <- as.integer(vapply(alice_orders, function(r) r$data$id, numeric(1)))
  expect_setequal(alice_order_ids, c(10L, 11L))
  # Sensitive column was not declared on OrderView, so it should not appear.
  expect_false("payment_token" %in% names(alice_orders[[1]]$data))

  order <- OrderView$get(id == 12)
  owner <- order$relationship("user")
  expect_true(inherits(owner, "Record"))
  expect_equal(owner$data$name, "Bob")
  expect_false("ssn" %in% names(owner$data))
})
