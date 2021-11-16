-- migrate:up
CREATE TABLE reply (
  topic_id
    BYTES NOT NULL REFERENCES topic (id),
  id
    BYTES,
  created_at
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  created_by
    BYTES NULL REFERENCES member (id),
  updated_at
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  updated_by
    BYTES NULL REFERENCES member (id),
  parent_reply_id
    BYTES NULL,
  deleted
    BOOL DEFAULT false NOT NULL,
  content
    STRING NOT NULL,
  PRIMARY KEY (topic_id, id),
  FOREIGN KEY (topic_id, parent_reply_id)
    REFERENCES reply (topic_id, id)
);
GRANT SELECT ON TABLE reply TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE reply TO api_write;

-- migrate:down
DROP TABLE reply;
