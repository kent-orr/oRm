testthat::source_test_helpers()

# End-to-end CRUD against a live SQL Server. Skips unless docker, the odbc
# package, and an ODBC driver for SQL Server are all present. Offline SQL
# generation is covered in test-Dialect-mssql.R.

withr::defer({
    cleanup_mssql_test_db()
}, testthat::teardown_env())

# =============================================================================
# CONNECTION AND DIALECT DETECTION
# =============================================================================

test_that("an odbc SQL Server connection is detected as the mssql dialect", {
    engine <- mssql_test_engine()
    withr::defer(engine$close())

    expect_equal(engine$dialect, "mssql")
    expect_true(DBI::dbIsValid(engine$get_connection()))
})

# =============================================================================
# CREATE TABLE
# =============================================================================

test_that("create_table is idempotent under if_not_exists", {
    engine <- mssql_test_engine()
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    model <- engine$model(
        "widgets",
        id = Column("INT IDENTITY(1,1)", primary_key = TRUE),
        name = Column("NVARCHAR(100)", nullable = FALSE)
    )

    expect_no_error(model$create_table())
    # The IF OBJECT_ID guard means a second call is a no-op, not an error.
    expect_no_error(model$create_table())

    expect_true("widgets" %in% oRm:::reflect_tables(engine, "dbo"))
})

# =============================================================================
# IDENTITY AND FLUSH (OUTPUT INSERTED.*)
# =============================================================================

test_that("IDENTITY keys are generated and returned on create", {
    engine <- mssql_test_engine()
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    model <- engine$model(
        "widgets",
        id = Column("INT IDENTITY(1,1)", primary_key = TRUE),
        name = Column("NVARCHAR(100)", nullable = FALSE)
    )
    model$create_table(overwrite = TRUE, ask = FALSE)

    rec <- model$record(name = "alpha")
    rec$create()

    # OUTPUT INSERTED.* hands back the server-generated key.
    expect_false(is.null(rec$data$id))
    expect_true(rec$data$id >= 1)
    expect_equal(rec$data$name, "alpha")

    second <- model$record(name = "beta")
    second$create()
    expect_true(second$data$id > rec$data$id)
})

test_that("a row of pure defaults inserts with DEFAULT VALUES", {
    engine <- mssql_test_engine()
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    con <- engine$get_connection()
    DBI::dbExecute(con, "DROP TABLE IF EXISTS dbo.defaults_tbl")
    # Every column is either IDENTITY or defaulted, so the insert carries no
    # values at all and must go down the DEFAULT VALUES path.
    DBI::dbExecute(con, paste(
        "CREATE TABLE dbo.defaults_tbl (",
        "id INT IDENTITY(1,1) PRIMARY KEY,",
        "label NVARCHAR(50) NULL DEFAULT 'unnamed')"
    ))

    model <- engine$reflect("defaults_tbl")
    rec <- model$record()
    rec$create()

    expect_true(rec$data$id >= 1)
    expect_equal(rec$data$label, "unnamed")
})

test_that("flush.mssql round-trips dates, timestamps and unicode text", {
    engine <- mssql_test_engine()
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    model <- engine$model(
        "typed_tbl",
        id = Column("INT IDENTITY(1,1)", primary_key = TRUE),
        label = Column("NVARCHAR(100)"),
        amount = Column("DECIMAL(10,2)"),
        flag = Column("BIT"),
        started_on = Column("DATE"),
        seen_at = Column("DATETIME2(0)")
    )
    model$create_table(overwrite = TRUE, ask = FALSE)

    rec <- model$record(
        label = "café – naïve",
        amount = 1234.56,
        flag = TRUE,
        started_on = as.Date("2024-03-01"),
        seen_at = as.POSIXct("2024-03-01 12:34:56", tz = "UTC")
    )
    rec$create()

    stored <- model$read(.mode = "data.frame")
    expect_equal(nrow(stored), 1L)
    expect_equal(stored$label, "café – naïve")
    expect_equal(as.numeric(stored$amount), 1234.56)
    expect_true(as.logical(stored$flag))
    expect_equal(as.Date(stored$started_on), as.Date("2024-03-01"))
})

test_that("flush.mssql quotes literals rather than interpolating them", {
    engine <- mssql_test_engine()
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    model <- engine$model(
        "quoting_tbl",
        id = Column("INT IDENTITY(1,1)", primary_key = TRUE),
        name = Column("NVARCHAR(200)")
    )
    model$create_table(overwrite = TRUE, ask = FALSE)

    nasty <- "O'Brien'); DROP TABLE quoting_tbl; --"
    rec <- model$record(name = nasty)
    rec$create()

    expect_equal(rec$data$name, nasty)
    expect_equal(nrow(model$read(.mode = "data.frame")), 1L)
})

# =============================================================================
# CRUD, INCLUDING COMPOSITE KEYS
# =============================================================================

test_that("records update, refresh and delete on an IDENTITY table", {
    engine <- mssql_test_engine()
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    model <- engine$model(
        "widgets",
        id = Column("INT IDENTITY(1,1)", primary_key = TRUE),
        name = Column("NVARCHAR(100)", nullable = FALSE)
    )
    model$create_table(overwrite = TRUE, ask = FALSE)
    model$record(name = "alpha")$create()
    model$record(name = "beta")$create()

    rec <- model$read(name == "alpha", .mode = "one")
    rec$data$name <- "alpha-updated"
    rec$update()
    expect_equal(model$read(id == rec$data$id, .mode = "one")$data$name, "alpha-updated")

    rec$data$name <- "scribbled locally"
    rec$refresh()
    expect_equal(rec$data$name, "alpha-updated")

    rec$delete()
    remaining <- model$read(.mode = "data.frame")
    expect_equal(nrow(remaining), 1L)
    expect_equal(remaining$name, "beta")
})

test_that("composite primary keys work end to end on SQL Server", {
    engine <- mssql_test_engine()
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    model <- engine$model(
        "memberships",
        org_id = Column("INT", primary_key = TRUE),
        user_id = Column("INT", primary_key = TRUE),
        role = Column("NVARCHAR(50)")
    )
    model$create_table(overwrite = TRUE, ask = FALSE)

    model$record(org_id = 1, user_id = 1, role = "admin")$create()
    model$record(org_id = 1, user_id = 2, role = "member")$create()
    model$record(org_id = 2, user_id = 1, role = "member")$create()

    expect_equal(length(model$read(.mode = "all")), 3L)

    rec <- model$read(org_id == 1, user_id == 2, .mode = "one")
    rec$data$role <- "owner"
    rec$update()

    expect_equal(model$read(org_id == 1, user_id == 2, .mode = "one")$data$role, "owner")
    expect_equal(model$read(org_id == 1, user_id == 1, .mode = "one")$data$role, "admin")

    model$read(org_id == 1, user_id == 2, .mode = "one")$delete()
    remaining <- model$read(.mode = "data.frame")
    expect_equal(nrow(remaining), 2L)
    expect_false(any(remaining$org_id == 1 & remaining$user_id == 2))
})

# =============================================================================
# SCHEMAS
# =============================================================================

test_that("schemas can be checked, created and written through", {
    engine <- mssql_test_engine()
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    expect_true(engine$check_schema_exists("dbo"))
    expect_false(engine$check_schema_exists("nope_not_here"))

    engine$create_schema("reporting")
    expect_true(engine$check_schema_exists("reporting"))
    # Creating an existing schema is a no-op, not an error.
    expect_no_error(engine$create_schema("reporting"))

    scoped <- Engine$new(
        drv = odbc::odbc(),
        Driver = mssql_driver_name(),
        Server = "localhost,1433",
        Database = MSSQL_TEST_DB,
        UID = "sa",
        PWD = MSSQL_TEST_PASSWORD,
        TrustServerCertificate = "yes",
        Encrypt = "no",
        .schema = "reporting"
    )
    withr::defer(scoped$close())

    model <- scoped$model(
        "metrics",
        id = Column("INT IDENTITY(1,1)", primary_key = TRUE),
        value = Column("INT")
    )
    expect_equal(model$tablename, "reporting.metrics")
    model$create_table(overwrite = TRUE, ask = FALSE)

    rec <- model$record(value = 42)
    rec$create()
    expect_equal(rec$data$value, 42)
    expect_true("metrics" %in% oRm:::reflect_tables(scoped, "reporting"))
})

# =============================================================================
# TRANSACTIONS
# =============================================================================

test_that("with.Engine commits and rolls back on SQL Server", {
    engine <- mssql_test_engine(persist = TRUE)
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    model <- engine$model(
        "tx_tbl",
        id = Column("INT", primary_key = TRUE),
        name = Column("NVARCHAR(50)")
    )
    model$create_table(overwrite = TRUE, ask = FALSE)

    with.Engine(engine, {
        model$record(id = 1, name = "committed")$create()
    })
    expect_equal(nrow(model$read(.mode = "data.frame")), 1L)

    expect_error(
        # with.Engine also warns as it rolls back; the error is what matters.
        suppressWarnings(with.Engine(engine, {
            model$record(id = 2, name = "rolled back")$create()
            stop("boom")
        })),
        "boom"
    )
    expect_equal(nrow(model$read(.mode = "data.frame")), 1L)
})

# =============================================================================
# TRIGGER FALLBACK (OUTPUT INSERTED is rejected on tables with triggers)
# =============================================================================

test_that("flush falls back to a keyed re-read on tables carrying triggers", {
    engine <- mssql_test_engine()
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    con <- engine$get_connection()
    DBI::dbExecute(con, "DROP TABLE IF EXISTS dbo.audited")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS dbo.audit_log")
    DBI::dbExecute(con, "CREATE TABLE dbo.audit_log (msg NVARCHAR(100))")
    DBI::dbExecute(con, "CREATE TABLE dbo.audited (id INT PRIMARY KEY, name NVARCHAR(100))")
    DBI::dbExecute(con, paste(
        "EXEC('CREATE TRIGGER trg_audited ON dbo.audited AFTER INSERT AS",
        "INSERT INTO dbo.audit_log (msg) VALUES (''inserted'')')"
    ))

    model <- engine$model(
        "audited",
        id = Column("INT", primary_key = TRUE),
        name = Column("NVARCHAR(100)")
    )

    # The key is client-supplied, so the fallback can re-read the row.
    rec <- model$record(id = 1, name = "with trigger")
    expect_no_error(rec$create())
    expect_equal(rec$data$name, "with trigger")
    expect_equal(nrow(model$read(.mode = "data.frame")), 1L)
})

test_that("flush recovers a server-generated IDENTITY key on tables carrying triggers", {
    engine <- mssql_test_engine()
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    con <- engine$get_connection()
    DBI::dbExecute(con, "DROP TABLE IF EXISTS dbo.audited_identity")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS dbo.audit_log2")
    DBI::dbExecute(con, "CREATE TABLE dbo.audit_log2 (msg NVARCHAR(100))")
    DBI::dbExecute(con, paste(
        "CREATE TABLE dbo.audited_identity (",
        "id INT IDENTITY(1,1) PRIMARY KEY, name NVARCHAR(100),",
        "created_at DATETIME2 NOT NULL DEFAULT SYSUTCDATETIME())"
    ))
    DBI::dbExecute(con, paste(
        "EXEC('CREATE TRIGGER trg_audited_identity ON dbo.audited_identity AFTER INSERT AS",
        "INSERT INTO dbo.audit_log2 (msg) VALUES (''inserted'')')"
    ))

    model <- engine$model(
        "audited_identity",
        id = Column("INT IDENTITY(1,1)", primary_key = TRUE),
        name = Column("NVARCHAR(100)"),
        created_at = Column("DATETIME2", nullable = TRUE)
    )

    # OUTPUT INSERTED is rejected (error 334), but a single IDENTITY key can
    # still be recovered with SCOPE_IDENTITY() and the full row re-read, so the
    # record comes back populated exactly as it would on a trigger-free table.
    rec <- model$record(name = "no key")
    expect_no_error(rec$create())
    expect_equal(rec$data$id, 1L)
    expect_equal(rec$data$name, "no key")
    expect_false(is.null(rec$data$created_at))

    # The trigger itself still fired.
    expect_equal(nrow(DBI::dbGetQuery(con, "SELECT * FROM dbo.audit_log2")), 1L)

    # The fallback toggles NOCOUNT inside its batch; that must not leak into
    # the session, or later statements would stop reporting affected rows.
    expect_equal(
        DBI::dbExecute(con, "UPDATE dbo.audited_identity SET name = 'renamed' WHERE id = 1"),
        1L
    )

    # The same recovery works inside a transaction (the #122 scenario), and a
    # rolled-back insert leaves no trace.
    rec2 <- with(engine, {
        r <- model$record(name = "in tx")$create(flush_record = TRUE)
        expect_equal(r$data$id, 2L)
        r
    })
    expect_equal(rec2$data$id, 2L)

    tryCatch(
        with(engine, {
            model$record(name = "rolled back")$create(flush_record = TRUE)
            stop("abort")
        }),
        error = function(e) NULL,
        warning = function(w) NULL
    )
    expect_equal(nrow(model$read(.mode = "data.frame")), 2L)
})

test_that("autoflush returns IDENTITY keys for every create inside a transaction", {
    engine <- mssql_test_engine()
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    parent <- engine$model(
        "orders",
        id = Column("INT IDENTITY(1,1)", primary_key = TRUE),
        customer = Column("NVARCHAR(100)")
    )
    child <- engine$model(
        "order_items",
        id = Column("INT IDENTITY(1,1)", primary_key = TRUE),
        order_id = Column("INT"),
        sku = Column("NVARCHAR(50)")
    )
    parent$create_table(overwrite = TRUE, ask = FALSE)
    child$create_table(overwrite = TRUE, ask = FALSE)

    # The #122 reprex: without autoflush the IDENTITY key stays NULL inside the
    # block, even though it is returned outside one.
    outside <- parent$record(customer = "outside")$create()
    expect_equal(outside$data$id, 1L)

    plain <- with(engine, parent$record(customer = "plain")$create())
    expect_null(plain$data$id)

    # With autoflush the key is available to the child insert in the same block.
    items <- with(engine, {
        order <- parent$record(customer = "Alice")$create()
        expect_false(is.null(order$data$id))
        child$record(order_id = order$data$id, sku = "A-1")$create()
    }, .autoflush = TRUE)

    expect_false(is.null(items$data$id))
    expect_equal(
        items$data$order_id,
        parent$read(customer == "Alice", .mode = "data.frame")$id
    )

    # Flushing joins the open transaction rather than committing on its own: a
    # failure rolls the flushed insert back with everything else.
    tryCatch(
        with(engine, {
            parent$record(customer = "rolled back")$create()
            stop("abort")
        }, .autoflush = TRUE),
        error = function(e) NULL,
        warning = function(w) NULL
    )
    expect_equal(nrow(parent$read(customer == "rolled back", .mode = "data.frame")), 0L)
})

test_that("an engine built with .autoflush = TRUE needs no per-block flag", {
    engine <- mssql_test_engine(.autoflush = TRUE)
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    parent <- engine$model(
        "orders2",
        id = Column("INT IDENTITY(1,1)", primary_key = TRUE),
        customer = Column("NVARCHAR(100)")
    )
    child <- engine$model(
        "order_items2",
        id = Column("INT IDENTITY(1,1)", primary_key = TRUE),
        order_id = Column("INT"),
        sku = Column("NVARCHAR(50)")
    )
    parent$create_table(overwrite = TRUE, ask = FALSE)
    child$create_table(overwrite = TRUE, ask = FALSE)

    # The #122 reprex verbatim, minus any flag at the call site.
    item <- with(engine, {
        order <- parent$record(customer = "Alice")$create()
        expect_equal(order$data$id, 1L)
        child$record(order_id = order$data$id, sku = "A-1")$create()
    })
    expect_equal(item$data$order_id, 1L)

    # A block can still opt out for a bulk load, restoring the engine default
    # afterwards.
    bulk <- with(engine, {
        parent$record(customer = "bulk")$create()
    }, .autoflush = FALSE)
    expect_null(bulk$data$id)
    expect_true(engine$get_autoflush())

    expect_equal(nrow(parent$read(.mode = "data.frame")), 2L)
})

test_that("flush reports a clear error when a trigger table key is neither supplied nor IDENTITY", {
    engine <- mssql_test_engine()
    withr::defer(clear_mssql_test_tables())
    withr::defer(engine$close())

    con <- engine$get_connection()
    DBI::dbExecute(con, "DROP TABLE IF EXISTS dbo.audited_guid")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS dbo.audit_log3")
    DBI::dbExecute(con, "CREATE TABLE dbo.audit_log3 (msg NVARCHAR(100))")
    DBI::dbExecute(con, paste(
        "CREATE TABLE dbo.audited_guid (",
        "id UNIQUEIDENTIFIER PRIMARY KEY DEFAULT NEWID(), name NVARCHAR(100))"
    ))
    DBI::dbExecute(con, paste(
        "EXEC('CREATE TRIGGER trg_audited_guid ON dbo.audited_guid AFTER INSERT AS",
        "INSERT INTO dbo.audit_log3 (msg) VALUES (''inserted'')')"
    ))

    model <- engine$model(
        "audited_guid",
        id = Column("UNIQUEIDENTIFIER", primary_key = TRUE),
        name = Column("NVARCHAR(100)")
    )

    # A server-defaulted, non-IDENTITY key cannot be recovered once OUTPUT is
    # off the table; the error must say why rather than surfacing raw error 334.
    expect_error(
        model$record(name = "no key")$create(),
        "trigger"
    )
})
