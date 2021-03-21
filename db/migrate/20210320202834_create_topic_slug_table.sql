-- migrate:up
CREATE TABLE topic_slug (
        slug
                STRING PRIMARY KEY,
        created
                TIMESTAMPTZ(0) NOT NULL DEFAULT current_timestamp(0),
        topic_id
                BYTES NOT NULL REFERENCES topic (id)
);

-- migrate:down
DROP TABLE topic_slug;
