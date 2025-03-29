test_that("Relationships define correctly", {
  devtools::load_all()
  engine <- Engine$new(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    persist = TRUE
  )

  User <- engine$model(
    "users",
    id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
    organization_id = ForeignKey('INTEGER', 'organizations.id'),
    name = Column("TEXT", nullable = FALSE),
    age = Column("INTEGER", default = 19)
  )


  Organization <- engine$model(
    "organizations",
    id = Column("INTEGER", primary_key = TRUE, nullable = FALSE),
    name = Column("TEXT", nullable = FALSE)
  )

  Organization$create_table()
  User$create_table() |>
    define_relationship('organization_id', 'many_to_one', Organization, 'id', ref = 'organization', backref = 'users')

  kent = User$record(id=1L, organization_id=1L, name="Kent", age = 34)$create()
  dylan = User$record(id = 2L, organization_id = 1L, name = "Dylan", age = 25)$create()
  orr = User$record(id = 3L, organization_id = 1L, name = "Orr")$create()

  widgets_inc = Organization$record(id=1L, name='Widgets, Inc')$create()

  kent$relationships
  kent$relationships[1] |> str()

  kent$relationship('organization')

  widgets_inc$relationships
  widgets_inc$relationship('users')

  expect_equal(kent$organization, widgets_inc)
  expect_equal(widgets_inc$users, list(kent))

User$fields |> str()

  User$relationship('organization')
  User$relationship('organization', id == 1)
  User$relationship('organization', id == 2)

  Organization$relationship('users', age < 30)
  Organization$relationship('users', dbplyr::sql("name like 'd%'"))
})