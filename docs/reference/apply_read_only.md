# Apply read-only enforcement to a freshly opened connection.

Dialect-specific dispatch. Called once per new physical connection when
\`Engine\$read_only\` is TRUE. SQLite handles read-only via the
SQLITE_RO open flag and so its method is a no-op.

## Usage

``` r
apply_read_only.mysql(x, con)

apply_read_only.postgres(x, con)

apply_read_only(x, con)

apply_read_only.default(x, con)

apply_read_only.sqlite(x, con)
```

## Arguments

- x:

  An Engine instance.

- con:

  A DBI connection object.

## Functions

- `apply_read_only.mysql()`: MySQL enforces read-only at the session
  level.

- `apply_read_only.postgres()`: PostgreSQL enforces read-only via the
  libpq \`options="-c default_transaction_read_only=on"\` connection
  parameter injected into \`conn_args\` at engine construction; nothing
  more to do post-connect.

- `apply_read_only.sqlite()`: SQLite enforces read-only via the
  SQLITE_RO open flag injected into \`conn_args\` at engine
  construction; nothing more to do post-connect.
