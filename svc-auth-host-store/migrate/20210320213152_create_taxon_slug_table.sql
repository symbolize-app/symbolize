-- migrate:up
CREATE TABLE taxon_slug (
  id
    BYTES NOT NULL REFERENCES taxon (id),
  language
    language NOT NULL,
  slug
    STRING NOT NULL,
  PRIMARY KEY (language, slug)
);
GRANT SELECT ON TABLE taxon_slug TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE taxon_slug TO api_write;

-- migrate:down
DROP TABLE taxon_slug;
