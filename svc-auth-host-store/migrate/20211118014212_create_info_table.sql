-- migrate:up
CREATE TABLE info (
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
  cross_language_id
    BYTES NOT NULL,
  title
    STRING DEFAULT '' NOT NULL,
  slug
    STRING DEFAULT '' NOT NULL,
  tags
    JSONB DEFAULT jsonb_build_array() NOT NULL,
  content
    STRING DEFAULT '' NOT NULL
);
CREATE INDEX info_cross_language_id_idx
  ON info (cross_language_id);
CREATE INDEX info_language_updated_at_idx
  ON info (language, updated_at);
CREATE INDEX info_created_by_deleted_updated_at_idx
  ON info (created_by, deleted, updated_at);
CREATE INDEX info_updated_by_deleted_updated_at_idx
  ON info (updated_by, deleted, updated_at);
GRANT SELECT ON TABLE info TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE info TO api_write;

-- migrate:down
DROP TABLE info;
