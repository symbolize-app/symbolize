# DB sharding

## Goals

- Small shards that can be quickly paged in from storage
- Small shards that can be quickly distributed during scale up
- Coherent data that does not always require multiple shards to build a display
- Coherent data that does not have shard-spanning transactions
- Simple, direct addressing to find a shard
- No background jobs or shard rebalancing
- Few enough shards to avoid exhausting file handle & DB connection resources
- Consistent policy that's easy to apply across services

## DBs

- auth
  - author
- blog
  - thread 
- cal
  - calendar
- chat
  - thread / day
- feed
  - author
- forum
  - thread
- letter
  - post
- log
  - service / day
- media
  - file
- mod
  - queue
- obj
  - none
- ping
  - author
- profile
  - author
- search
  - none
- site
  - none
- task
  - task
- repo
  - repository
- review
  - review
- track
  - service / day
- vote
  - vote

## Indexes

- Every sharded DB needs a global index
- Ideally the index doesn't contain much besides the shard ID
- Do not want write contention here
- Service instance that has write access to index also creates new shard DBs
- Index is used for FTS queue (sort by update time), and for display (paging, sort by create time)

## Notes

- Some DBs contain unstructured streams
  - These need to be partitioned by day
  - Shard ID is still a GUID, gets registered in an index
- Some DBs contain recursive threads
  - Each thread still partitioned separately even though only some are top-level
  - Links to messages must include thread ID (DB shard ID)
- All DB shards have a capacity limit
  - For example, max notifications or feed size per author, or max thread messages per day (chat) or all time (forum)
  - This preserves DB shard performance
  - Needs special UI treatment (e.g. "please create a new thread")
- To preserve resources, some DBs do not allow subscriptions
  - For example, media
