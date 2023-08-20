-- migrate:up
CREATE TABLE reply (
  id
    BYTES PRIMARY KEY,
  created_at
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  created_by
    BYTES NULL REFERENCES member (id),
  updated_at
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  updated_by
    BYTES NULL REFERENCES member (id),
  deleted
    BOOL DEFAULT true NOT NULL,
  language
    language NOT NULL,
  subforum_id
    BYTES NOT NULL REFERENCES subforum (id),
  topic_id
    BYTES NOT NULL REFERENCES topic (id),
  parent_reply_id
    BYTES DEFAULT NULL REFERENCES reply (id),
  slug
    STRING NOT NULL,
  tags
    JSONB DEFAULT jsonb_build_array() NOT NULL,
  content
    STRING DEFAULT '' NOT NULL
);
CREATE INDEX reply_language_updated_at_idx
  ON reply (language, updated_at);
CREATE INDEX reply_created_by_deleted_updated_at_idx
  ON reply (created_by, deleted, updated_at);
CREATE INDEX reply_updated_by_deleted_updated_at_idx
  ON reply (updated_by, deleted, updated_at);
GRANT SELECT ON TABLE reply TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE reply TO api_write;

-- migrate:down
DROP TABLE reply;
