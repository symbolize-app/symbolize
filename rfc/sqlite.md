# SQLite conventions

## Error handling

- Enable enforcement of foreign-key constraints
- Use strict tables for rigid typing
- Disallow nulls for primary keys
  - Not needed for integers or tables without row IDs
- Don't use autoincrement primary keys
  - So integer primary keys can't be used as row IDs

## Unique IDs

- UUIDv4 as blob
  - Ideally all 16 bytes random
  - Also use a method-local random source, for multi-threading and unit testing
  - Collisions astronomically unlikely, should be handled with standard error/retry
- For primary keys and untrusted client request IDs

## Secure tokens

- Securely randomly generated as blob

## Timestamps

- Epoch seconds as integer
  - Auto-expands up to 8 bytes, gives 238,000,000,000 years left
- To break ties, add a sequence integer

## Bookkeeping

- Created at, created by
- Updated at, updated by
- Archived, archived at, archived by
- Deleted, deleted at, deleted by
- Revision

## Write-Ahead Logs

- Disable WAL auto-checkpoint
  - Manually trigger when safe
- Open DB in immutable mode for readers
- Set journal mode to "delete" if permanently saving a DB

## Tuning

- Enable memory-mapped IO
- Recommended to optimize DB on every connection close
- Consider vacuuming
- Set locking mode to exclusive
- Open DB in "no mutex" mode

## References

- https://www.sqlite.org/datatype3.html
- https://www.sqlite.org/stricttables.html
- https://www.sqlite.org/wal.html
