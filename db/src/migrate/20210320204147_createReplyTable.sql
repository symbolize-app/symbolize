-- migrate:up
CREATE TABLE reply (
  topic_id
    BYTES NOT NULL REFERENCES topic (id),
  id
    BYTES,
  created
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  updated
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  deleted
    TIMESTAMPTZ(0) NULL,
  member_id
    BYTES NOT NULL REFERENCES member (id),
  parent_reply_id
    BYTES NULL,
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
