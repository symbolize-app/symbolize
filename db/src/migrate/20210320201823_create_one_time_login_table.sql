-- migrate:up
CREATE TABLE one_time_login (
  id
    BYTES PRIMARY KEY,
  created_at
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  deleted_at
    TIMESTAMPTZ(0) DEFAULT NULL NULL,
  member_id
    BYTES NOT NULL REFERENCES member (id),
  session_id
    BYTES DEFAULT NULL NULL REFERENCES session (id)
);
CREATE INDEX one_time_login_member_id_deleted_at_created_at_idx
  ON one_time_login (member_id, deleted_at, created_at);
GRANT SELECT ON TABLE one_time_login TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE one_time_login TO api_write;

-- migrate:down
DROP TABLE one_time_login;
