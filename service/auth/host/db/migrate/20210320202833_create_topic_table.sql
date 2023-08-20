-- migrate:up
CREATE TABLE topic (
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
  bumped_at
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  bumped_by
    BYTES NULL REFERENCES member (id),
  deleted
    BOOL DEFAULT true NOT NULL,
  language
    language NOT NULL,
  subforum_id
    BYTES NOT NULL REFERENCES subforum (id),
  title
    STRING DEFAULT '' NOT NULL,
  slug
    STRING DEFAULT '' NOT NULL,
  tags
    JSONB DEFAULT jsonb_build_array() NOT NULL,
  content
    STRING DEFAULT '' NOT NULL
);
CREATE INDEX topic_language_updated_at_idx
  ON topic (language, updated_at);
CREATE INDEX topic_subforum_id_bumped_at_idx
  ON topic (subforum_id, bumped_at) WHERE deleted = false;
CREATE INDEX topic_created_by_deleted_bumped_at_idx
  ON topic (created_by, deleted, bumped_at);
CREATE INDEX topic_updated_by_deleted_bumped_at_idx
  ON topic (updated_by, deleted, bumped_at);
CREATE INDEX topic_bumped_by_deleted_bumped_at_idx
  ON topic (bumped_by, deleted, bumped_at);
GRANT SELECT ON TABLE topic TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE topic TO api_write;

-- migrate:down
DROP TABLE topic;
