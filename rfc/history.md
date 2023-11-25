# History

For record keeping, completeness, and auditing, a history of all changes is preserved.

## Tasks

- [ ] Allow unbounded growth?
- [ ] Separate/parallel repo service for storage/display of other services' histories? Or embedded as a library?

## Resources

- https://phiresky.github.io/blog/2022/sqlite-zstd/

## Performance

It would be possible to store history as separate DB rows (or embedded columns), those seem costly for either updates (if compressed) or total size (if uncompressed).

Instead, use a system built for history: Git. It will work well for history-related queries too.

To reduce overhead, each DB shard has one shared Git repo embedded in the SQLite database. The embedding is done at the file-level (for compatibilty with the Git CLI) and exposed via FUSE.

The first time Git is needed for a shard, the filesystem is mounted and the database is opened (via libgit2). These can stay open for a while, in case future requests also need Git.

(Write access to Git will need to be serialized, but this might be taken care of with the SQLite client anyway.)

For storing Git files in SQLite, one row should be one filesystem page.

## File layout

Try to keep it simple: one folder per table, one folder per row, one file per column. This way columns (like message text) are their own files for easy history parsing. Non-text columns are stored in whatever format is used for writing to the main SQLite rows.

## Draft changes

Per shard, each author's draft changes are stored in a Git branch matching their author ID. Before loading the changes next time, the branch is rebased to main.
