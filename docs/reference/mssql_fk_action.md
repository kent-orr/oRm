# Map a SQL Server referential-action description to a SQL action

\`sys.foreign_keys\` reports actions as \`NO_ACTION\`, \`CASCADE\`,
\`SET_NULL\` or \`SET_DEFAULT\`. \`NO_ACTION\` is the implicit default
and is returned as NULL so it is not rendered.

## Usage

``` r
mssql_fk_action(desc)
```
