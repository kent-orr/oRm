library(stevedore)

# Setup PostgreSQL container for testing
setup_postgres_test_db <- function() {
    # Check if stevedore is available
    if (!requireNamespace("stevedore", quietly = TRUE)) {
        install.packages("stevedore")
        if (!requireNamespace("stevedore", quietly = TRUE)) {
            testthat::skip("stevedore package not available, skipping PostgreSQL tests")
        }
    }
    
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

test_that('The Postgres dialect works as expected', {
    # Skip if RPostgres is not available
    if (!requireNamespace("RPostgres", quietly = TRUE)) {
        skip("RPostgres not available, skipping PostgreSQL tests")
    }
    
    # Get connection info from the Docker container
    conn_info <- tryCatch({
        setup_postgres_test_db()
    }, error = function(e) {
        skip(paste("Could not set up PostgreSQL container:", e$message))
        NULL
    })
    
    if (is.null(conn_info)) {
        skip("PostgreSQL container setup failed")
    }
    
    # Create engine with the connection info
    engine <- do.call(Engine$new, conn_info)
    
    expect_equal(engine$dialect, 'postgres')
    
    expect_no_error({
        engine$get_connection()
        engine$close()
    })
    
    TempUser <- engine$model(
        "temp_users",
        id = Column("SERIAL", primary_key = TRUE),
        name = Column("TEXT", nullable = FALSE),
        age = Column("INTEGER")
    )
    
    TempUser$create_table(overwrite = TRUE)
    expect_true('temp_users' %in% engine$list_tables())
    
    p1 = TempUser$record(id=1, name='test_person', age=19)
    p1$create()
    p1user = TempUser$read(id==1, mode='get')
    expect_equal(p1, p1user)
    
    TempUser <- engine$model(
        "temp_users",    # Use a unique name even for temp tables
        id = Column("SERIAL", primary_key = TRUE, nullable=FALSE),
        name = Column("TEXT", nullable = FALSE),
        age = Column("INTEGER")
    )
  
    TempUser$create_table(overwrite=TRUE, verbose=TRUE)
    p1 = TempUser$record(name='John', age = 18)
    p1$create()
    
    # Test SERIAL auto-increment
    p2 = TempUser$record(name='Jane', age = 25)
    p2$create()
    
    # Read back and verify auto-increment worked
    all_users = TempUser$read(mode='all')
    expect_equal(length(all_users), 2)
    
    # Clean up
    TempUser$drop_table(ask = FALSE)
})
