

test_that('The Postgres dialect works as expected', {

    engine <- Engine$new(
      drv = RPostgres::Postgres(),
      dbname = "test",
      host = "localhost",
      user = "tester",
      password = "tester"
    )

    expect_equal(engine$dialect, 'postgres')

    expect_no_error({
      engine$get_connection()
      engine$close()
    })

    TempUser <- engine$model(
      "temp_users",
      id = Column("SERIAL", primary_key = TRUE, nullable = FALSE),
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
      "temp_users",  # Use a unique name even for temp tables
      id = Column("SERIAL", primary_key = TRUE, nullable=FALSE),
      name = Column("TEXT", nullable = FALSE),
      age = Column("INTEGER")
    )

    TempUser$create_table(overwrite=TRUE, verbose=TRUE)
    p1 = TempUser$record(name='John', age = 18)
    p1$create()



})
