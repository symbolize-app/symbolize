-- migrate:up
CREATE TABLE tag_slug (
  language
    language NOT NULL,
  slug
    STRING NOT NULL,
  tag_id
    BYTES NOT NULL REFERENCES tag (id),
  PRIMARY KEY (language, slug)
);
GRANT SELECT ON TABLE tag_slug TO api_read;
GRANT SELECT, INSERT ON TABLE tag_slug TO api_write;

-- migrate:down
DROP TABLE tag_slug;
