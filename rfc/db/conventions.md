# DB conventions

## Error handling

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
