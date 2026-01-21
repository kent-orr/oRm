# Changelog

## oRm 0.3.0

### New Features

- Added comprehensive PostgreSQL dialect support with RETURNING clause
- Enhanced schema management with automatic schema creation
- Improved transaction handling with
  [`with.Engine()`](https://kent-orr.github.io/oRm/reference/with.Engine.md)
  method
- Added support for connection pooling via pool package

### Improvements

- Enhanced dialect system for better database compatibility
- Improved error handling and validation
- Better documentation and vignettes
- Added comprehensive test coverage for PostgreSQL dialect

### Bug Fixes

- Fixed S3 method signature consistency for
  [`with()`](https://rdrr.io/r/base/with.html) methods
- Improved handling of NULL values in database operations
- Fixed documentation issues and parameter mismatches

### Breaking Changes

- Updated
  [`with.Engine()`](https://kent-orr.github.io/oRm/reference/with.Engine.md)
  and
  [`with.Record()`](https://kent-orr.github.io/oRm/reference/with.Record.md)
  method signatures for S3 consistency

------------------------------------------------------------------------

This is the initial CRAN submission for oRm.
