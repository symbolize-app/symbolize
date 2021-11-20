-- migrate:up
CREATE TABLE reply_history (
  reply_id
    BYTES NOT NULL REFERENCES reply (id),
  updated_at
    TIMESTAMPTZ(0) NOT NULL,
  updated_by
    BYTES NULL REFERENCES member (id),
  deleted
    BOOL NOT NULL,
  published
    BOOL NOT NULL,
  content
    STRING NOT NULL,
  PRIMARY KEY (reply_id, updated_at)
);
GRANT SELECT ON TABLE reply_history TO api_read;
GRANT SELECT, INSERT ON TABLE reply_history TO api_write;

-- migrate:down
DROP TABLE reply_history;
