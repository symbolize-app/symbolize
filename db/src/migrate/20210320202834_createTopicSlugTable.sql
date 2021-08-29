-- migrate:up
CREATE TABLE topic_slug (
  slug
    STRING PRIMARY KEY,
  created
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  topic_id
    BYTES NOT NULL REFERENCES topic (id)
);
GRANT SELECT ON TABLE topic_slug TO api_read;
GRANT SELECT, INSERT ON TABLE topic_slug TO api_write;

-- migrate:down
DROP TABLE topic_slug;
