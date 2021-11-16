-- migrate:up
CREATE TABLE subforum_slug (
  language
    language NOT NULL,
  slug
    STRING NOT NULL,
  subforum_id
    BYTES NOT NULL REFERENCES subforum (id),
  PRIMARY KEY (language, slug)
);
GRANT SELECT ON TABLE subforum_slug TO api_read;
GRANT SELECT, INSERT ON TABLE subforum_slug TO api_write;

-- migrate:down
DROP TABLE subforum_slug;
