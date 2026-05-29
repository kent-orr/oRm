# oRm Testing Checklist

This document outlines testing gaps identified in the oRm package and
provides a checklist for implementing comprehensive test coverage.

## 🔴 Critical Gaps (High Priority)

### MySQL Dialect Testing

**Create `test-Dialect-mysql.R`** - Currently missing entirely

Test
[`flush.mysql()`](https://kent-orr.github.io/oRm/reference/flush.md)
function with `LAST_INSERT_ID()` behavior

Test
[`qualify.mysql()`](https://kent-orr.github.io/oRm/reference/qualify.md)
schema qualification

Test
[`set_schema.mysql()`](https://kent-orr.github.io/oRm/reference/set_schema.md)
with `USE` statement (Note: PostgreSQL schema switching is disabled)

Test MySQL connection initialization and dialect detection

Test MySQL-specific data types (BIGINT, TINYINT, etc.)

Test MySQL auto-increment behavior

Test MySQL transaction handling and commit/rollback

### ForeignKey Class Testing

**Create comprehensive ForeignKey tests**

Test ForeignKey constructor with various reference formats

Test reference string parsing (“table.column” format)

Test ForeignKey constraint validation

Test integration with TableModel constraint generation

Test ForeignKey with different data types

Test ForeignKey with NULL values and cascading options

## 🟡 Important Gaps (Medium Priority)

### Error Handling & Edge Cases

**Connection Failures**

Network timeout scenarios

Authentication failures

Database server unavailability

Invalid connection parameters

**Schema Operations**

Operations on non-existent schemas

Schema permission restrictions

Reserved keyword schema names

Schema naming conflicts

**Constraint Violations**

Foreign key constraint violations

UNIQUE constraint violations

NOT NULL constraint violations

CHECK constraint violations

**Data Validation**

Invalid data type coercion

Boundary values for numeric types

Oversized string values

Invalid date/time formats

### Transaction Handling

**Nested Transactions**

Nested
[`with.Engine()`](https://kent-orr.github.io/oRm/reference/with.Engine.md)
blocks

Savepoint creation and rollback

Transaction state management with nesting

**Complex Transaction Scenarios**

Long-running transaction timeouts

Deadlock detection and handling

Mixed commit/rollback operations

Transaction isolation level testing

**Concurrent Access**

Multi-user scenarios

Concurrent read/write operations

Connection pool under concurrent load

### Relationship Edge Cases

**Complex Relationships**

Self-referencing relationships (User -\> User)

Circular relationship dependencies

Multi-hop relationship traversal

**Many-to-Many Relationships**

Automatic junction table creation

Junction table naming conventions

Junction table with additional fields

Many-to-many record deletion cascades

**Relationship Updates**

Updating foreign key references

Orphaned record handling

Relationship consistency validation

## 🟢 Nice-to-Have (Lower Priority)

### Schema Management Advanced Cases

**Cross-Schema Operations**

Foreign keys across different schemas

Relationships between models in different schemas

Schema migration with existing data

**Schema Permissions**

Testing with restricted schema access

Schema creation permission failures

Read-only schema access scenarios

### Performance & Scalability

**Large Dataset Operations**

Bulk insert performance (1000+ records)

Memory usage with large result sets

Query optimization with complex filters

**Pagination Edge Cases**

Very large offset values (\> 10000)

Negative limit boundary conditions

Pagination with complex WHERE clauses

Performance degradation with deep pagination

### Connection & Pool Management

**Pool Configuration**

Different pool sizes (1, 5, 20+ connections)

Pool timeout settings

Pool validation intervals

**Connection Lifecycle**

Stale connection detection

Connection recovery after network issues

Pool exhaustion scenarios

Graceful pool shutdown

### Data Integrity

**NULL Handling**

NULL values in primary keys (should fail)

NULL values in foreign keys

NULL values with default functions

NULL value coercion across data types

**Default Value Execution**

Function-based defaults under high load

Default value consistency across transactions

Default values with complex data types

**Column Constraints**

UNIQUE constraint across multiple columns

Complex CHECK constraint validation

Constraint naming and error messages

### Integration & Compatibility

**dplyr Integration**

Complex dplyr operations with `tbl` mode

dplyr joins with oRm relationships

dplyr aggregations and grouping

Performance comparison: dplyr vs oRm methods

**R6 Method Chaining**

Complex method chaining scenarios

Error propagation through chains

Memory management in long chains

**Package Dependencies**

Different versions of DBI drivers

dbplyr version compatibility

pool package version compatibility

### Security & Validation

**SQL Injection Prevention**

Malicious input in WHERE clauses

SQL injection attempts in field names

Parameterization of all user inputs

Special characters in table/column names

**Input Sanitization**

Script injection attempts

Unicode and special character handling

Binary data handling

**Privilege Testing**

Operations with limited database permissions

Read-only user scenarios

Schema-restricted access testing

## Testing Implementation Priority

1.  **Phase 1**: Create missing MySQL dialect tests and ForeignKey tests
2.  **Phase 2**: Implement comprehensive error handling tests
3.  **Phase 3**: Add complex transaction and relationship scenarios
4.  **Phase 4**: Performance, security, and edge case testing

## Notes for Test Implementation

- Use
  [`testthat::skip_if_not_installed()`](https://testthat.r-lib.org/reference/skip.html)
  for database-specific tests
- Mock external dependencies where appropriate
- Use [`withr::defer()`](https://withr.r-lib.org/reference/defer.html)
  for proper test cleanup
- Test both success and failure scenarios for each feature
- Include performance benchmarks for critical operations
- Document any database-specific setup requirements

## Test Coverage Goals

- **Minimum**: 90% line coverage across all R files
- **Target**: 95% line coverage with comprehensive edge cases
- **Critical**: 100% coverage for security-sensitive functions (SQL
  generation, parameterization)
