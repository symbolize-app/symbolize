-- migrate:up
CREATE TABLE taxon (
  id
    BYTES PRIMARY KEY,
  created
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  updated
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  deleted
    TIMESTAMPTZ(0) NULL,
  member_id
    BYTES NOT NULL REFERENCES member (id),
  parent_taxon_id
    BYTES NULL REFERENCES taxon (id),
  rank
    rank NOT NULL,
  names
    JSONB NOT NULL,
  slug
    STRING NOT NULL,
  intro
    STRING NOT NULL
);
GRANT SELECT ON TABLE taxon TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE taxon TO api_write;

-- migrate:down
DROP TABLE taxon;
