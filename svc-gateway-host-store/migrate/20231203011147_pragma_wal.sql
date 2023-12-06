-- migrate:up transaction:false

pragma journal_mode = wal;

-- migrate:down

pragma journal_mode = delete;
