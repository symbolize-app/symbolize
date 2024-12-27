# DB subscriptions

Some read operations will be result in asynchronous streams and be implemented as DB subscriptions.

When a read service gets an update to a shard, it will re-execute any relevant subscription queries, and send results that are found.
