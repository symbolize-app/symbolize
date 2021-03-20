-- migrate:up
CREATE TABLE topic_slug (
        slug
                TEXT PRIMARY KEY,
        created
                TIMESTAMPTZ(3) NOT NULL DEFAULT current_timestamp(3),
        topic_id
                UUID NOT NULL REFERENCES topic (id)
);

-- migrate:down
DROP TABLE topic_slug;
