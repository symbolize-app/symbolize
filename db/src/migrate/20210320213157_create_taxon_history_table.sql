-- migrate:up
CREATE TABLE taxon_history (
  taxon_id
    BYTES NOT NULL REFERENCES taxon (id),
  updated_at
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  updated_by
    BYTES NULL REFERENCES member (id),
  deleted
    BOOL NOT NULL,
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
    STRING NOT NULL,
  PRIMARY KEY (taxon_id, updated_at)
);
GRANT SELECT ON TABLE taxon_history TO api_read;
GRANT SELECT, INSERT ON TABLE taxon_history TO api_write;

-- migrate:down
DROP TABLE taxon_history;
