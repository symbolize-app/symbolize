# DB checkpointing

Periodically, the write leader will pick a resident shard and perform checkpointing and vacuuming.

There will be a minimum threshold of drift (where an idle shard will be picked) and a maximum threshold (which will knowingly cause disruption). Drift will include both WAL size and age.
