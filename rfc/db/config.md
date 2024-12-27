# DB config

## Referential integrity

- Enable enforcement of foreign-key constraints
- Use strict tables for rigid typing

## Write-Ahead Logs

- Disable WAL auto-checkpoint and checkpoint-on-close
- Open DB in immutable mode for readers

## Tuning

- Enable memory-mapped IO
- Set locking mode to exclusive
- Open DB in "no mutex" mode

## References

- https://www.sqlite.org/datatype3.html
- https://www.sqlite.org/stricttables.html
- https://www.sqlite.org/wal.html
