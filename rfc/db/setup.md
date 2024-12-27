# DB setup

Instead of running a DB as a remote service, the DB will be embedded with the main web service. And instead of having a single DB, data will aggressively sharded into multiple DBs.

This will have multiple effects:

- Reduced contention for writes
- Highly scalable reads and subscriptions
- No network latency for reads
- Limited operation times via limited shard sizes

In the spirit of simplicity, DB will run on SQLite, and backups will be asynchronously synced to an object store.

To achieve distributed reads, all new entries in the WAL will be sent from the write leader to all followers.
