-- migrate:up
CREATE TABLE topic_history (
        topic_id
                UUID NOT NULL REFERENCES topic (id),
        updated
                TIMESTAMPTZ(3) NOT NULL,
        title
                TEXT NOT NULL,
        slug
                TEXT NOT NULL,
        content
                TEXT NOT NULL,
        PRIMARY KEY (topic_id, updated)
);

-- migrate:down
DROP TABLE topic_history;
