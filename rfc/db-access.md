# DB access

The goal here is to keep data distributed and avoid contention bottlenecks, reducing latency for both reads and writes.

Sharding, paging, local copies, and delayed reads are the main techniques.

## Origin

The central storage for all data will be FoundationDB, with a separate cluster used for each service. When writes are committed, they go here.

Schema basics:

- Active writer and readers for each shard
- Monotonic version number for each shard
- One key per page per file
- All keys in one data shard use the same key prefix

## Backup

Periodically, FoundationDB will scanned (or a secondary index used) and data will be synced to Tigris. Once in Tigris, it can also be evicted from FoundationDB. Syncing and evicting will be done per-page.

FoundationDB backups will also be used.

## Read

"Read" services will only perform read-only operations. When a page needs to be used, they will request it from (in priority order)...

- A random active reader for the shard
- The active writer for the shard
- FoundationDB
- Tigris (also write page to FoundationDB)

If read-after-write consistency is needed, a shard version number can be given to a read query, and the reader will wait until it's at least at that version before attempting the query.

## Write

"Write" services can perform write operations. When they use a shard, they need to acquire exclusive write ownership of the shard.

As a consistency check, the writer makes sure that its initial shard version number matches the shard version in FoundationDB when committing.

To populate a page, a write service only uses FoundationDB (or Tigris) as the source.

After writing a page, the service also checks if there are any active readers. If so, it notifies them that the page has changed (maybe sending the page contents).

## Relational

Relational data will be stored in SQLite databases, one shard as one DB, that are themselves stored in FoundationDB (separate files for DB and log). A service will open a "local" DB on a FUSE mount, and the FUSE handler will transparently handle reads and writes (with its own caching layer).

For this to work properly when a write and readers are all active, the DB needs to be set to WAL mode with auto-checkpointing and checkpoint-on-close disabled. When new WAL entries are available, SQLite will be able to see the new data. The rest of the DB file content will remain immutable (can be enforce by FUSE handler).

Periodically, FoundationDB will be scanned (or a secondary index used) and DBs will be checkpointed (applying the WAL to the main DB content). This needs exclusive write access, and readers will need to reset their DB connections afterward (forcing fresh reads for all cached pages).

Becuase SQLite and FUSE do not use async IO, one thread will be needed for each parallel query. Only one write thread needed per DB (could revisit), but multiple read threads would be good for performance.

## Code

Code data will be stored in Git repos, one shard as one repo, that are themselves stored in FoundationDB (separate files mirroring repo structure, refs stored in separate keys). A service will open a "local" DB on a FUSE mount, and the FUSE handler will transparently handle reads and writes (with its own caching layer).

For this to work properly when a write and readers are all active, Git auto-GC needs to be disabled. When new objects are available, Git will be able to see them as new files. All of the previously-written files in the Git repo will remain immutable (with refs not stored/updated in Git).

Periodically, FoundationDB will be scanned (or a secondary index used) and repos will be GC'd (optimizing, compression, and cleanup). This needs exclusive write access, and readers will need to reset any in-memory repos afterward (forcing fresh reads for all cached pages).

Becuase Git (including even Gitoxide and libgit2) and FUSE do not use async IO for local repos, one thread will be needed for each parallel operation. Only one write thread needed per repo (could revisit), but multiple read threads would be good for performance.

## Blob

Blob data will be stored as a single append-only file, one shard as one file.
