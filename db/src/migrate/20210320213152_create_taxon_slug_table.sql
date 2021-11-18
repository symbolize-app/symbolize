-- migrate:up
CREATE TABLE taxon_slug (
  language
    language NOT NULL,
  slug
    STRING NOT NULL,
  taxon_id
    BYTES NOT NULL REFERENCES taxon (id),
  PRIMARY KEY (language, slug)
);
GRANT SELECT ON TABLE taxon_slug TO api_read;
GRANT SELECT, INSERT ON TABLE taxon_slug TO api_write;

-- migrate:down
DROP TABLE taxon_slug;
