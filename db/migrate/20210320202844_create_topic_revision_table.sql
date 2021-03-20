-- migrate:up
CREATE TABLE topic_revision (
        topic_id
                UUID DEFAULT gen_random_uuid(),
        revision
                INT4 NOT NULL,
        is_latest
                BOOL NOT NULL,
        created
                TIMESTAMPTZ(3) NOT NULL DEFAULT current_timestamp(3),
        title
                TEXT NOT NULL,
        slug
                TEXT NULL UNIQUE,
        content
                TEXT NOT NULL,
        PRIMARY KEY (topic_id, revision)
);

-- migrate:down
DROP TABLE topic_revision;
