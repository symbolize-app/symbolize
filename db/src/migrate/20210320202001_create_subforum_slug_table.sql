-- migrate:up
CREATE TABLE subforum_slug (
  id
    BYTES NOT NULL REFERENCES subforum (id),
  language
    language NOT NULL,
  slug
    STRING NOT NULL,
  PRIMARY KEY (language, slug)
);
GRANT SELECT ON TABLE subforum_slug TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE subforum_slug TO api_write;

-- migrate:down
DROP TABLE subforum_slug;
