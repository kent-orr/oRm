# oRm Roadmap: Schema Migrations

Status: proposal / not started. Target: post-0.6.0.

## Why

In R, "managing migrations" today means `{pool}`/`{DBI}` + a folder of raw SQL,
or shelling out to a language-agnostic tool (Flyway, Liquibase, Alembic). oRm is
uniquely positioned to do better because it **already reflects a live database
into the same model objects you author by hand** (`engine$reflect()` /
`reflect_schema()`). That means both sides of any migration — the declared model
and the actual database — are available as comparable R objects. No other R ORM
realistically has this.

The strategic bet: the headline feature is **schema diff / autogenerate** (M2),
built on a small set of **`ALTER` primitives** (M1), made credible by a
**versioned migration runner** (M3).

## Current surface (what we build on)

- `Engine`: `execute_sql()`, `create_schema()`, `reflect()`, `reflect_schema()`,
  `read_only` guard, transaction-capable connection (DBI/pool).
- `TableModel`: `create_table(if_not_exists, overwrite, ask)`, `drop_table(ask)`,
  set-level CRUD (`create`/`update`/`delete` with `.all` guard + `ask` confirm).
- `Column` / `ForeignKey`: typed fields with `primary_key`, `nullable`, `unique`,
  `default`; per-dialect rendering via `render_field()` / `render_constraint()`.
- Dialects: sqlite, postgres, mysql.

### Gaps blocking migrations

1. No incremental DDL — only whole-table CREATE/DROP. Changes are destructive.
2. No `ALTER` path (`ADD`/`DROP`/`RENAME`/`ALTER COLUMN`).
3. No version tracking or migration history.
4. No diff between declared model and reflected DB.
5. No FK-ordered bootstrap/teardown of a model set.

## Milestones

### M1 — `ALTER` primitives on `TableModel` (enabling work)

The missing half of the DDL spine; mirrors the existing verb convention so it's
discoverable via `$`-autocomplete.

- `Model$add_column(name, Column(...))`
- `Model$drop_column(name)`
- `Model$rename_column(old, new)`
- `Model$alter_column(name, ...)` — type/null/default changes
- Each reuses `render_field()` / `render_constraint()`; thin dialect-aware
  wrappers emitting `ALTER TABLE`.
- Honor `read_only` guard and `ask`/confirm pattern for destructive ops.
- **SQLite caveat:** limited `ALTER` support → implement the create-new /
  copy / drop / rename table-rebuild dance behind the same interface.

Deliverable: incremental DDL works on all three dialects; tests per dialect
(SQLite in-memory always; PG/MySQL gated like existing suite).

### M2 — Schema diff / autogenerate (headline feature)

Compare a declared model against its reflected counterpart and reconcile.

- `engine$diff(Model)` → structured diff object: added / dropped / changed
  columns, constraint deltas, type changes. Symmetric over reflection output so
  declared-vs-live is a pure object comparison.
- `engine$plan(Model)` (or `diff$plan()`) → the ordered `ALTER` statements (from
  M1) that reconcile DB → model. Dry-runnable; prints SQL without executing.
- `engine$diff_schema()` / `plan_schema()` → whole-schema version using
  `reflect_schema()` and FK-dependency ordering.
- Explicitly scope what diffing covers in v1 (columns, types, nullability,
  defaults, PK/FK/unique) vs. deferred (indexes, check constraints, triggers).

Deliverable: `plan()` produces correct, dialect-valid SQL for the common change
set; round-trip test (declare → create → mutate model → plan → apply → reflect →
diff is empty).

### M3 — Versioned migration runner (credibility feature)

What turns "DDL helpers" into "migrations" people trust in production.

- Managed history table `orm_migrations` (version, name, applied_at, checksum).
- Migrations as R functions with `up(engine)` / `down(engine)` steps; ordered by
  version; recorded only on success.
- `engine$migrate()` applies pending; `engine$current_version()` reports state;
  `engine$rollback(to = ...)` runs `down()` steps.
- Refuse to run on a `read_only` engine (reuse existing guard).
- Optional: `engine$generate_migration(name)` scaffolds an `up`/`down` file from
  `plan()` output — the Alembic `--autogenerate` workflow, but in R.

Deliverable: a project can track DB version, apply/rollback ordered migrations,
and recover state after interruption.

### M4 — Transactional, safe-by-default execution

- Wrap each migration in a transaction where the dialect supports
  transactional DDL (PG, SQLite). Surface clearly that **MySQL largely does
  not** — document the auto-commit-per-statement risk.
- Reuse `.all = TRUE` / `ask` confirmation semantics so a migration can't
  silently drop data.
- On failure mid-migration: roll back the transaction (or, on MySQL, report the
  partial-apply state explicitly via the history checksum).

### M5 — Reproducible bootstrap / teardown

- `engine$create_all(models)` / `drop_all(models)` in FK-dependency order.
- The topological sort is half-built: `reflect_schema()` already resolves FK
  wiring across a model set — factor that ordering out for reuse.

## Sequencing & dependencies

```
M1 (ALTER primitives) ──┬─> M2 (diff/autogenerate) ──> M3 (runner) ──> M4 (txn safety)
                        └─> M5 (bootstrap, independent, low effort)
```

M1 is the smallest self-contained start and unblocks everything else. M5 can land
anytime (it only composes existing CREATE/DROP + FK ordering). M2 is the
differentiator and should be the public "why oRm for migrations" story. M3 + M4
are table stakes users expect before trusting it on a real database.

## Open questions

- Migration definition format: R functions only, or also declarative/SQL files?
- Should `plan()` ever auto-apply, or always require an explicit `migrate()`?
- Index / check-constraint / trigger coverage in diff — v1 scope vs. deferred.
- MySQL non-transactional DDL: document-and-warn, or attempt emulation?
- Naming: align verbs with existing convention (`create`/`update`/`delete`,
  `reflect`) — e.g. `add_column` vs `create_column`. Revisit against
  [[crud-verb-mirroring-convention]].
