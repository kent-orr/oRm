# Map a PostgreSQL referential-action code to a SQL action.

\`pg_constraint.confdeltype\`/\`confupdtype\` are single characters. "NO
ACTION" (\`a\`) is the implicit default and is returned as NULL so it is
not rendered.

## Usage

``` r
pg_fk_action(code)
```
