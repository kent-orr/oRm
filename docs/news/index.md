# Changelog

## oRm (development version)

### New Features

- Added [`Method()`](https://kent-orr.github.io/oRm/reference/Method.md)
  function for attaching custom methods to models at both table and
  record levels
  - Table-level methods operate on the entire table for custom queries
    and bulk operations
  - Record-level methods operate on individual records for
    instance-specific behavior
  - Both method types have access to `self` for calling model/record
    methods
- Added automatic JSON serialization/deserialization for PostgreSQL
  - JSON and JSONB columns automatically serialize R objects when
    writing to database
  - Automatically deserialize JSON to R objects when reading from
    database

### Documentation

- Added new vignette “Using Methods” demonstrating method usage patterns
- Updated existing vignettes (Get Started, Using Records, Using
  TableModels) to include method documentation
- Added `Method` to core building blocks in package documentation

### Improvements

- Dropped stevedore dependency for PostgreSQL test setup

------------------------------------------------------------------------

## oRm 0.3.1

Initial CRAN release.
