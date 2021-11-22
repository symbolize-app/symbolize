-- migrate:up
CREATE TABLE reply_history (
  id
    BYTES NOT NULL REFERENCES reply (id),
  saved_at
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  saved_by
    BYTES NULL REFERENCES member (id),
  scheduled_at
    TIMESTAMPTZ(0) DEFAULT NULL NULL,
  updated_at
    TIMESTAMPTZ(0) DEFAULT NULL NULL,
  latest
    BOOL DEFAULT true NOT NULL,
  deleted
    BOOL DEFAULT false NOT NULL,
  content
    STRING NOT NULL,
  PRIMARY KEY (id, saved_at)
);
CREATE INDEX reply_history_scheduled_at_not_updated_idx
  ON reply_history (scheduled_at)
  WHERE
    latest = true
    AND scheduled_at IS NOT NULL
    AND updated_at IS NULL;
CREATE INDEX reply_history_saved_by_updated_at_latest_scheduled_at_idx
  ON reply_history (
    saved_by,
    updated_at,
    latest,
    scheduled_at
  );
GRANT SELECT ON TABLE reply_history TO api_read;
GRANT SELECT, INSERT ON TABLE reply_history TO api_write;

-- migrate:down
DROP TABLE reply_history;
