-- migrate:up
CREATE TABLE topic_slug (
  language
    language NOT NULL,
  slug
    STRING NOT NULL,
  topic_id
    BYTES NOT NULL REFERENCES topic (id),
  PRIMARY KEY (language, slug)
);
GRANT SELECT ON TABLE topic_slug TO api_read;
GRANT SELECT, INSERT ON TABLE topic_slug TO api_write;

-- migrate:down
DROP TABLE topic_slug;
