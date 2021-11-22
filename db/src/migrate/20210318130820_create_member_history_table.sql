-- migrate:up
CREATE TABLE member_history (
  id
    BYTES NOT NULL REFERENCES member (id),
  updated_at
    TIMESTAMPTZ(0) NOT NULL,
  updated_by
    BYTES NULL REFERENCES member (id),
  delete_scheduled_at
    TIMESTAMPTZ(0) NULL,
  "role"
    role NOT NULL,
  private
    BOOL NOT NULL,
  email
    STRING NOT NULL,
  handle
    STRING NOT NULL,
  PRIMARY KEY (id, updated_at)
);
GRANT SELECT ON TABLE member_history TO api_read;
GRANT SELECT, INSERT ON TABLE member_history TO api_write;

-- migrate:down
DROP TABLE member_history;
