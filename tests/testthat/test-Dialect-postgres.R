
# Setup PostgreSQL container for testing
setup_postgres_test_db <- function() {
    # Check if stevedore is available
    if (!requireNamespace("stevedore", quietly = TRUE)) {
        install.packages("stevedore")
        if (!requireNamespace("stevedore", quietly = TRUE)) {
            testthat::skip("stevedore package not available, skipping PostgreSQL tests")
        }
    } 
  
    library(stevedore)
    
    # Check if Docker is available
    if (!stevedore::docker_available()) {
        testthat::skip("Docker not available, skipping PostgreSQL tests")
    }
    
    # Connect to Docker
    docker <- stevedore::docker_client()
    
    # Define container name
    container_name <- "orm_postgres_test"
    
    message("Pulling PostgreSQL Alpine image, this may take a while...")
    docker$image$pull("postgres:14-alpine")
    
    # Create and start a new container
    
    docker <- docker_client()
    
    container_name <- "orm_postgres_test"
    tryCatch({
        existing_container <- docker$container$get(container_name)
        message("Stopping existing container...")
        existing_container$stop()
        existing_container$remove(force=TRUE)
    }, error = function(e) {
        # Container doesn't exist, which is fine
    })
  
    container <- docker$container$create(
        name = container_name,
        image = "postgres:14-alpine",
        env = c(
            POSTGRES_USER = "tester",
            POSTGRES_PASSWORD = "tester",
            POSTGRES_DB = "test"
        ),
        ports = c("5432:5432")
    ) 
  
    container$start()
    
    # Wait for PostgreSQL to be ready
    message("Waiting for PostgreSQL to start...")
    Sys.sleep(5)
    
    # Return connection info
    list(
        drv = RPostgres::Postgres(),
        dbname = "test",
        host = "localhost",
        user = "tester",
        password = "tester",
        port = 5432
    )
}

cleanup_postgres_test_db <- function() {
    if (!requireNamespace("stevedore", quietly = TRUE) || !stevedore::docker_available()) {
        return(invisible(NULL))
    }
    
    docker <- stevedore::docker_client()
    container_name <- "orm_postgres_test"  # Match the name used in setup
    
    tryCatch({
        container <- docker$container$get(container_name)
        container$stop()
        container$remove()
    }, error = function(e) invisible(NULL))
}

# Register cleanup function to run when R exits
reg.finalizer(environment(), function(e) {
    cleanup_postgres_test_db()
}, onexit = TRUE)

get_postgres_engine <- function() {
    if (!requireNamespace("RPostgres", quietly = TRUE)) {
        testthat::skip("RPostgres not available, skipping PostgreSQL tests")
    }

    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        testthat::skip(paste("Could not set up PostgreSQL container:", e$message))
    })

    do.call(Engine$new, conn_info)
}

test_that("postgres engine initializes and closes", {
    engine <- get_postgres_engine()
    withr::defer(engine$close())

    expect_equal(engine$dialect, "postgres")
    expect_no_error(engine$get_connection())
    expect_no_error(engine$close())
})

test_that("postgres create operations work", {
    engine <- get_postgres_engine()
    withr::defer(engine$close())

    TempUser <- engine$model(
        "temp_users",
        id = Column("SERIAL", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE),
        age = Column("INTEGER")
    )

    TempUser$create_table(overwrite = TRUE)
    withr::defer(TempUser$drop_table(ask = FALSE))
    expect_true("temp_users" %in% engine$list_tables())

    p1 <- TempUser$record(name = "John", age = 18)
    p1$create()
    expect_equal(p1$data$id, 1)

    p2 <- TempUser$record(name = "Jane", age = 25)
    p2$create()
    expect_equal(p2$data$id, 2)

    all_users <- TempUser$read(mode = "all")
    expect_equal(length(all_users), 2)
})

test_that("postgres read operations work", {
    engine <- get_postgres_engine()
    withr::defer(engine$close())

    TempUser <- engine$model(
        "temp_users",
        id = Column("SERIAL", primary_key = TRUE),
        name = Column("TEXT", nullable = FALSE),
        age = Column("INTEGER")
    )

    TempUser$create_table(overwrite = TRUE)
    withr::defer(TempUser$drop_table(ask = FALSE))

    p1 <- TempUser$record(id = 1, name = "test_person", age = 19)
    p1$create()
    p1_user <- TempUser$read(id == 1, mode = "get")
    expect_equal(p1, p1_user)
})

test_that("postgres update/refresh works", {
    engine <- get_postgres_engine()
    withr::defer(engine$close())

    TempUser <- engine$model(
        "temp_users",
        id = Column("SERIAL", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE),
        age = Column("INTEGER")
    )

    TempUser$create_table(overwrite = TRUE)
    withr::defer(TempUser$drop_table(ask = FALSE))

    p1 <- TempUser$record(name = "John", age = 18)
    p1$create()

    engine$execute("UPDATE temp_users SET age = 30 WHERE id = 1")
    p1$refresh()
    db_user <- engine$get_query("SELECT * FROM temp_users WHERE id = 1")
    expect_equal(p1$data, as.list(db_user[1, ]))
})

test_that("postgres delete operations work", {
    engine <- get_postgres_engine()
    withr::defer(engine$close())

    TempUser <- engine$model(
        "temp_users",
        id = Column("SERIAL", primary_key = TRUE, nullable = FALSE),
        name = Column("TEXT", nullable = FALSE),
        age = Column("INTEGER")
    )

    TempUser$create_table(overwrite = TRUE)
    withr::defer(TempUser$drop_table(ask = FALSE))

    p1 <- TempUser$record(name = "John", age = 18)
    p1$create()
    p2 <- TempUser$record(name = "Jane", age = 25)
    p2$create()

    p1$delete()
    expect_equal(length(TempUser$read(id == 1, mode = "all")), 0)
    remaining_users <- TempUser$read(mode = "all")
    expect_equal(length(remaining_users), 1)
})

test_that("postgres schema switching works", {
    engine <- get_postgres_engine()
    withr::defer(engine$close())

    DBI::dbExecute(engine$get_connection(), "CREATE SCHEMA IF NOT EXISTS audit")
    withr::defer(DBI::dbExecute(engine$get_connection(), "DROP SCHEMA IF EXISTS audit CASCADE"))

    engine$set_schema("audit")
    withr::defer(engine$set_schema("public"))

    AuditUser <- engine$model(
        "audit_users",
        id = Column("SERIAL", primary_key = TRUE),
        name = Column("TEXT", nullable = FALSE)
    )

    AuditUser$create_table(overwrite = TRUE)
    withr::defer(AuditUser$drop_table(ask = FALSE))
    expect_true("audit.audit_users" %in% engine$list_tables())
})
