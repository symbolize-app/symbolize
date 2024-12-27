# DB replication

A single instance will be the write leader for all DB shards. All write requests will either go directly to that instance, or get proxied by followers.

## Lease

If the leader becomes unreachable, a new leader will need to be selected. Any instance can take the write leader lease if the previous leader's lease has expired. An leader will continuously renew its lease while active.

This will be done with Lua scripts and an expiring key in Redis.

If a leader ever finds that that its lease expires (local timer) or its lease renewal fails (external transaction), it should cancel all active and pending requests and then exit to avoid data corruption.

## Handoff

To hand off to a new leader, the current leader needs to block all write requests, then select any active follower. After that, new write requests will get forwarded to the new leader.

Handoff should be a safe process that can be run at any time.

It can also be run in the context of shutdown:

1. Hand off to new leader
2. Deregister from load balancer
3. Drain all requests, streams, and connections

# Transfer

A non-public HTTP port will be used for serving replication streams. After every commit, new WAL entries will te transferred to all active followers. To make the transfers idempotent, some form of checksums or length checks could be used.

The WAL of a follower can be written while there are active connections, but these changes won't be automatically seen in new queries. Instead, all active connections need to close, and new connections will see the changes.

## Integrity

Replication will be a synchronous operation, and a commit will not be considered successful until the change has been accepted by all followers (as a protection against lost writes).

## Startup

When a follower starts up, it will need to get the full contents of all shards. This will block any checkpointing, and will proceed up to the last page for all shards. Then, a full lock on all shards will be taken, and the final page for all shards will be transferred, promoting the follower from pending to active status.

Until the follower is fully active, it will not accept any external requests. This avoids needing to proxy subscriptions.

If a pending follower is takes the leader lease, it will all synced shards and start from scratch. If an active follower takes the leader lease, it can continue without discarding any data.

## Extension: multiple leaders

If a single write leader is getting overloaded, leadership can be split out to multiple instances. This could be as fine-grained as "every active shard can have a separate leader", or it could be something more structured.

## Extension: indirect followers

If synchronous follower writes are causing too much overhead, asynchronous, indirect followers can be added. They'd follow the direct followers and would not be eligible for handoff (and would need to follow a different instance if their direct follower becomes the leader).

## Extension: live WAL-index file update

There may be a way to force a refresh of the WAL-index (shared memory) file for incorporating remote changes. This could involve safely updating the file while respecting locks, or triggering an internal reconciliation process.

## Alternatives to Redis-based lease

Instead of Redis, it would be fine to use etcd or Fly.io's built in Consul cluster. Or even embedding Raft consensus into the app. But for now, Redis is the simplest solution.
