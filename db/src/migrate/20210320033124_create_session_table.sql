-- migrate:up
CREATE TABLE session (
  id
    BYTES PRIMARY KEY,
  created_at
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  last_active_at
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  deleted_at
    TIMESTAMPTZ(0) DEFAULT NULL NULL,
  member_id
    BYTES NOT NULL REFERENCES member (id),
  browser
    STRING NOT NULL,
  os
    STRING NOT NULL,
  countries
    JSONB NOT NULL,
  languages
    JSONB NOT NULL,
  recent_activity
    JSONB
    DEFAULT jsonb_build_array(current_timestamp(0)::STRING)
    NOT NULL
);
CREATE INDEX session_member_id_deleted_at_last_active_at_idx
  ON session (member_id, deleted_at, last_active_at);
GRANT SELECT ON TABLE session TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE session TO api_write;

-- migrate:down
DROP TABLE session;
