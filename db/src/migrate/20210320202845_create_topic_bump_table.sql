-- migrate:up
CREATE TABLE topic_bump (
  id
    BYTES NOT NULL REFERENCES topic (id),
  created_at
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  created_by
    BYTES NULL REFERENCES member (id),
  deleted_at
    TIMESTAMPTZ(0) DEFAULT NULL NULL,
  PRIMARY KEY (id, created_at)
);
CREATE INDEX topic_bump_created_by_deleted_at_created_at_idx
  ON topic_bump (created_by, deleted_at, created_at);
GRANT SELECT ON TABLE topic_bump TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE topic_bump TO api_write;

-- migrate:down
DROP TABLE topic_bump;
