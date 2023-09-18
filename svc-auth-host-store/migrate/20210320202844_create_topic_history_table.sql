-- migrate:up
CREATE TABLE topic_history (
  id
    BYTES NOT NULL REFERENCES topic (id),
  saved_at
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  saved_by
    BYTES NULL REFERENCES member (id),
  scheduled_at
    TIMESTAMPTZ(0) DEFAULT NULL NULL,
  in_progress_at
    TIMESTAMPTZ(0) DEFAULT NULL NULL,
  updated_at
    TIMESTAMPTZ(0) DEFAULT NULL NULL,
  latest
    BOOL DEFAULT true NOT NULL,
  deleted
    BOOL DEFAULT false NOT NULL,
  subforum_id
    BYTES NOT NULL REFERENCES subforum (id),
  title
    STRING NOT NULL,
  slug
    STRING NOT NULL,
  tags
    JSONB NOT NULL,
  content
    STRING NOT NULL,
  PRIMARY KEY (id, saved_at)
);
CREATE INDEX topic_history_scheduled_at_not_updated_idx
  ON topic_history (scheduled_at)
  WHERE
    latest = true
    AND scheduled_at IS NOT NULL
    AND updated_at IS NULL;
CREATE INDEX topic_history_saved_by_updated_at_latest_scheduled_at_idx
  ON topic_history (
    saved_by,
    updated_at,
    latest,
    scheduled_at
  );
GRANT SELECT ON TABLE topic_history TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE topic_history TO api_write;

-- migrate:down
DROP TABLE topic_history;
