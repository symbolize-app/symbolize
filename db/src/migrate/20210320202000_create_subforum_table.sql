-- migrate:up
CREATE TABLE subforum (
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
CREATE INDEX subforum_cross_language_id_idx
  ON subforum (cross_language_id);
CREATE INDEX subforum_created_by_idx
  ON subforum (created_by);
CREATE INDEX subforum_updated_by_idx
  ON subforum (updated_by);
GRANT SELECT ON TABLE subforum TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE subforum TO api_write;

-- migrate:down
DROP TABLE subforum;
