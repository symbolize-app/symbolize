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
    BOOL DEFAULT false NOT NULL,
  published
    BOOL NOT NULL,
  language
    language NOT NULL,
  cross_language_id
    BYTES NOT NULL,
  title
    STRING NOT NULL,
  slug
    STRING NOT NULL,
  tags
    JSONB NOT NULL,
  content
    STRING NOT NULL
);
CREATE INDEX ON info (cross_language_id);
GRANT SELECT ON TABLE info TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE info TO api_write;

-- migrate:down
DROP TABLE info;
