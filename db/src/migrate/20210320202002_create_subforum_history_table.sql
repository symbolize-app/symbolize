-- migrate:up
CREATE TABLE subforum_history (
  id
    BYTES NOT NULL REFERENCES subforum (id),
  updated_at
    TIMESTAMPTZ(0) NOT NULL,
  updated_by
    BYTES NULL REFERENCES member (id),
  deleted
    BOOL NOT NULL,
  cross_language_id
    BYTES NOT NULL,
  name
    STRING NOT NULL,
  slug
    STRING NOT NULL,
  description
    STRING NOT NULL,
  PRIMARY KEY (id, updated_at)
);
CREATE INDEX subforum_history_updated_by_idx
  ON subforum_history (updated_by);
GRANT SELECT ON TABLE subforum_history TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE subforum_history TO api_write;

-- migrate:down
DROP TABLE subforum_history;
