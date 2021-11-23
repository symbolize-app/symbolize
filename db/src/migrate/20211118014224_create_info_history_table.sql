-- migrate:up
CREATE TABLE info_history (
  id
    BYTES NOT NULL REFERENCES info (id),
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
  cross_language_id
    BYTES NOT NULL,
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
CREATE INDEX info_history_scheduled_at_not_updated_idx
  ON info_history (scheduled_at)
  WHERE
    latest = true
    AND scheduled_at IS NOT NULL
    AND updated_at IS NULL;
CREATE INDEX info_history_saved_by_updated_at_latest_scheduled_at_idx
  ON info_history (
    saved_by,
    updated_at,
    latest,
    scheduled_at
  );
GRANT SELECT ON TABLE info_history TO api_read;
GRANT SELECT, INSERT ON TABLE info_history TO api_write;

-- migrate:down
DROP TABLE info_history;
