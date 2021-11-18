-- migrate:up
CREATE TABLE topic_history (
  topic_id
    BYTES NOT NULL REFERENCES topic (id),
  updated_at
    TIMESTAMPTZ(0) NOT NULL,
  updated_by
    BYTES NULL REFERENCES member (id),
  deleted
    BOOL NOT NULL,
  published
    BOOL NOT NULL,
  title
    STRING NOT NULL,
  slug
    STRING NOT NULL,
  tags
    JSONB NOT NULL,
  content
    STRING NOT NULL,
  PRIMARY KEY (topic_id, updated_at)
);
GRANT SELECT ON TABLE topic_history TO api_read;
GRANT SELECT, INSERT ON TABLE topic_history TO api_write;

-- migrate:down
DROP TABLE topic_history;
