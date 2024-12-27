# DB backup

## Paging

The write leader will maintain an LRU cache of DB shards, and all shards will be distributed to all followers.

## Layout

Because values in an object store are immutable, there needs to be a mapping from SQLite's WAL.

The approach here is to manage the main DB and WAL using one key per page, and an index file for committing changes. Updates to a page will trigger a new object version, and the index file will record the hashes and versions of each page.

Hashes in the index will be used later to avoid conditional writes to the object store during later full backups (after checkpointing).

Version assertions will be used for writing to the index file to avoid a worst-case scenario of overwriting a new index with an old index (even though multiple backup writers should not be running simultaneously).

## Liveness

Backups will be async, but the system will have a fixed tolerance for lag, after which all writes will be blocked until backups catch up.
