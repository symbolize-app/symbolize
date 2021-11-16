-- migrate:up
CREATE TABLE tag (
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
  cross_language_id
    BYTES NOT NULL,
  name
    STRING NOT NULL,
  slug
    STRING NOT NULL,
  description
    STRING NOT NULL
);
CREATE INDEX ON tag (cross_language_id);
GRANT SELECT ON TABLE tag TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE tag TO api_write;

-- migrate:down
DROP TABLE tag;
