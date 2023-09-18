-- migrate:up
CREATE TABLE topic_slug (
  id
    BYTES NOT NULL REFERENCES topic (id),
  language
    language NOT NULL,
  slug
    STRING NOT NULL,
  PRIMARY KEY (language, slug)
);
GRANT SELECT ON TABLE topic_slug TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE topic_slug TO api_write;

-- migrate:down
DROP TABLE topic_slug;
