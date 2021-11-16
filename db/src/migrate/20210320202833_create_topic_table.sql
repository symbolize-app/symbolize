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
  deleted
    BOOL DEFAULT false NOT NULL,
  language
    language NOT NULL,
  title
    STRING NOT NULL,
  slug
    STRING NOT NULL,
  tags
    JSONB NOT NULL,
  content
    STRING NOT NULL
);
GRANT SELECT ON TABLE topic TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE topic TO api_write;

-- migrate:down
DROP TABLE topic;
