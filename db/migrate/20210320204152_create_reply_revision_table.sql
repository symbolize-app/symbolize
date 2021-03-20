-- migrate:up
CREATE TABLE reply_revision (
        reply_id
                UUID NOT NULL REFERENCES topic (id) PRIMARY KEY,
        revision
                INT4 NOT NULL,
        is_latest
                BOOL NOT NULL,
        created
                TIMESTAMPTZ(3) NOT NULL DEFAULT current_timestamp(3),
        content
                TEXT NOT NULL
);

-- migrate:down
DROP TABLE reply_revision;
