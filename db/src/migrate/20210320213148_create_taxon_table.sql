-- migrate:up
CREATE TABLE taxon (
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
  parent_taxon_id
    BYTES NULL REFERENCES taxon (id),
  language
    language NOT NULL,
  cross_language_id
    BYTES NOT NULL,
  rank
    rank NOT NULL,
  names
    JSONB NOT NULL,
  slug
    STRING NOT NULL,
  intro
    STRING NOT NULL
);
CREATE INDEX ON taxon (cross_language_id);
GRANT SELECT ON TABLE taxon TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE taxon TO api_write;

-- migrate:down
DROP TABLE taxon;
