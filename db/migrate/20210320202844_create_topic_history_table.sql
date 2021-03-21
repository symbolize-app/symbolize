-- migrate:up
CREATE TABLE topic_history (
        topic_id
                BYTES NOT NULL REFERENCES topic (id),
        updated
                TIMESTAMPTZ(0) NOT NULL,
        title
                STRING NOT NULL,
        slug
                STRING NOT NULL,
        content
                STRING NOT NULL,
        PRIMARY KEY (topic_id, updated)
);

-- migrate:down
DROP TABLE topic_history;
