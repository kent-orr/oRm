testthat::source_test_helpers()

# Unit tests for the mssql dialect that need no SQL Server instance. The
# `.dialect` override lets these run over a real SQLite connection, so
# identifier quoting is genuine while the generated T-SQL is only inspected,
# never executed. Live-server behaviour is covered in
# test-Dialect-mssql-integration.R and test-reflect-mssql.R.

mssql_offline_engine <- function() {
  skip_if_not_installed("RSQLite")
  Engine$new(
    drv = RSQLite::SQLite(), dbname = ":memory:",
    persist = TRUE, .dialect = "mssql"
  )
}

# ============================================================================
# DISPATCH
# ============================================================================

test_that("dialect dispatch resolves the mssql implementations", {
  engine <- mssql_offline_engine()
  withr::defer(engine$close())

  expect_equal(engine$dialect, "mssql")
  expect_identical(oRm:::flush.mssql, get0("flush.mssql", envir = asNamespace("oRm")))
  for (m in c("flush", "reflect_columns", "reflect_tables", "sql_create_table",
              "check_schema_exists", "create_schema", "set_schema", "apply_read_only")) {
    expect_true(
      is.function(get0(paste0(m, ".mssql"), envir = asNamespace("oRm"))),
      info = paste("missing", m, ".mssql")
    )
  }
})

# ============================================================================
# CREATE TABLE (T-SQL has no CREATE TABLE IF NOT EXISTS)
# ============================================================================

test_that("mssql create_table guards existence with IF OBJECT_ID", {
  engine <- mssql_offline_engine()
  withr::defer(engine$close())

  model <- engine$model(
    "users",
    id = Column("INT IDENTITY(1,1)", primary_key = TRUE),
    name = Column("NVARCHAR(100)")
  )

  sql <- model$create_table(verbose = TRUE)

  expect_match(sql, "IF OBJECT_ID(", fixed = TRUE)
  expect_match(sql, "N'U'", fixed = TRUE)
  expect_match(sql, "IS NULL", fixed = TRUE)
  expect_match(sql, "BEGIN", fixed = TRUE)
  expect_match(sql, "END", fixed = TRUE)
  expect_match(sql, "CREATE TABLE", fixed = TRUE)
  # The ANSI spelling is not valid T-SQL and must not appear.
  expect_false(grepl("IF NOT EXISTS", sql, fixed = TRUE))
})

test_that("mssql create_table emits a bare CREATE TABLE when not guarding", {
  engine <- mssql_offline_engine()
  withr::defer(engine$close())

  model <- engine$model("users", id = Column("INT", primary_key = TRUE))
  sql <- model$create_table(verbose = TRUE, if_not_exists = FALSE)

  expect_match(sql, "CREATE TABLE", fixed = TRUE)
  expect_false(grepl("IF OBJECT_ID", sql, fixed = TRUE))
  expect_false(grepl("BEGIN", sql, fixed = TRUE))
})

test_that("IDENTITY types pass through DDL untouched", {
  engine <- mssql_offline_engine()
  withr::defer(engine$close())

  model <- engine$model(
    "users",
    id = Column("INT IDENTITY(1,1)", primary_key = TRUE),
    name = Column("NVARCHAR(100)", nullable = FALSE)
  )
  sql <- model$create_table(verbose = TRUE)

  expect_match(sql, "INT IDENTITY(1,1) PRIMARY KEY", fixed = TRUE)
  expect_match(sql, "NVARCHAR(100) NOT NULL", fixed = TRUE)
})

test_that("composite primary keys render as a table constraint under mssql", {
  engine <- mssql_offline_engine()
  withr::defer(engine$close())

  model <- engine$model(
    "memberships",
    org_id = Column("INT", primary_key = TRUE),
    user_id = Column("INT", primary_key = TRUE),
    role = Column("NVARCHAR(50)")
  )
  sql <- model$create_table(verbose = TRUE)

  expect_equal(lengths(regmatches(sql, gregexpr("PRIMARY KEY", sql)))[[1]], 1L)
  expect_match(sql, "PRIMARY KEY (", fixed = TRUE)
  expect_match(sql, "IF OBJECT_ID(", fixed = TRUE)
})

# ============================================================================
# TYPE FORMATTING (reflection helper, pure function)
# ============================================================================

test_that("mssql_format_type renders lengths, precision and scale", {
  f <- oRm:::mssql_format_type

  expect_equal(f("int", max_length = 4, precision = 10, scale = 0), "int")
  expect_equal(f("bit", max_length = 1, precision = 1, scale = 0), "bit")

  # Byte lengths: varchar is 1 byte/char, nvarchar 2.
  expect_equal(f("varchar", max_length = 50, precision = 0, scale = 0), "varchar(50)")
  expect_equal(f("nvarchar", max_length = 200, precision = 0, scale = 0), "nvarchar(100)")
  expect_equal(f("char", max_length = 10, precision = 0, scale = 0), "char(10)")
  expect_equal(f("nchar", max_length = 20, precision = 0, scale = 0), "nchar(10)")

  # -1 means MAX.
  expect_equal(f("varchar", max_length = -1, precision = 0, scale = 0), "varchar(MAX)")
  expect_equal(f("nvarchar", max_length = -1, precision = 0, scale = 0), "nvarchar(MAX)")
  expect_equal(f("varbinary", max_length = -1, precision = 0, scale = 0), "varbinary(MAX)")

  expect_equal(f("decimal", max_length = 9, precision = 10, scale = 2), "decimal(10,2)")
  expect_equal(f("numeric", max_length = 9, precision = 18, scale = 4), "numeric(18,4)")

  expect_equal(f("datetime2", max_length = 8, precision = 27, scale = 7), "datetime2(7)")
  expect_equal(f("datetime", max_length = 8, precision = 23, scale = 3), "datetime")
})

test_that("mssql_format_type appends IDENTITY when requested", {
  f <- oRm:::mssql_format_type

  expect_equal(
    f("int", max_length = 4, precision = 10, scale = 0,
      is_identity = TRUE, seed = 1, increment = 1),
    "int IDENTITY(1,1)"
  )
  expect_equal(
    f("bigint", max_length = 8, precision = 19, scale = 0,
      is_identity = TRUE, seed = 100, increment = 5),
    "bigint IDENTITY(100,5)"
  )
})

# ============================================================================
# FOREIGN KEY ACTIONS (reflection helper, pure function)
# ============================================================================

test_that("mssql_fk_action maps referential actions", {
  f <- oRm:::mssql_fk_action

  # NO_ACTION is the implicit default and is dropped so it is not rendered.
  expect_null(f("NO_ACTION"))
  expect_null(f(NA_character_))
  expect_equal(f("CASCADE"), "CASCADE")
  expect_equal(f("SET_NULL"), "SET NULL")
  expect_equal(f("SET_DEFAULT"), "SET DEFAULT")
})

# ============================================================================
# IDENTITY / AUTO-GENERATED FIELD DETECTION
# ============================================================================

test_that("is_auto_generated_type recognizes IDENTITY and the SERIAL family", {
  f <- oRm:::is_auto_generated_type

  expect_true(f("SERIAL"))
  expect_true(f("BIGSERIAL"))
  expect_true(f("SMALLSERIAL"))
  expect_true(f("serial"))

  expect_true(f("INT IDENTITY(1,1)"))
  expect_true(f("int identity(1,1)"))
  expect_true(f("BIGINT IDENTITY"))

  expect_false(f("INTEGER"))
  expect_false(f("TEXT"))
  expect_false(f("NVARCHAR(100)"))
  # Substring matches must not count.
  expect_false(f("IDENTITYCARD"))
  expect_false(f("SERIALIZED"))
})

test_that("required-field validation skips IDENTITY columns", {
  engine <- mssql_offline_engine()
  withr::defer(engine$close())

  model <- engine$model(
    "users",
    id = Column("INT IDENTITY(1,1)", primary_key = TRUE, nullable = FALSE),
    name = Column("NVARCHAR(100)", nullable = FALSE)
  )

  # `id` is generated by the server, so omitting it is not a missing field;
  # omitting `name` still is.
  rec <- model$record(name = "alpha")
  expect_false("id" %in% names(rec$data))
  expect_error(model$record()$create(), "Missing required fields: name")
})

# ============================================================================
# SCHEMA HANDLING
# ============================================================================

test_that("mssql schema qualification uses name prefixing", {
  engine <- mssql_offline_engine()
  withr::defer(engine$close())

  expect_equal(oRm:::qualify(engine, "users", "dbo"), "dbo.users")
  # Already-qualified names are left alone.
  expect_equal(oRm:::qualify(engine, "sales.users", "dbo"), "sales.users")
  expect_equal(oRm:::qualify(engine, "users", NULL), "users")
})

test_that("mssql set_schema is a no-op", {
  engine <- mssql_offline_engine()
  withr::defer(engine$close())

  # T-SQL has no session-level schema switch; qualification does the work.
  expect_silent(oRm:::set_schema(engine, "dbo"))
  expect_null(oRm:::set_schema(engine, "dbo"))
})

test_that("mssql read-only mode warns that enforcement is application-level", {
  skip_if_not_installed("RSQLite")
  engine <- Engine$new(
    drv = RSQLite::SQLite(), dbname = ":memory:",
    persist = TRUE, .dialect = "mssql"
  )
  withr::defer(engine$close())

  expect_warning(
    oRm:::apply_read_only(engine, engine$get_connection()),
    "read-only"
  )
})
