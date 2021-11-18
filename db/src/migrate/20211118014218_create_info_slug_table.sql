-- migrate:up
CREATE TABLE info_slug (
  language
    language NOT NULL,
  slug
    STRING NOT NULL,
  info_id
    BYTES NOT NULL REFERENCES info (id),
  PRIMARY KEY (language, slug)
);
GRANT SELECT ON TABLE info_slug TO api_read;
GRANT SELECT, INSERT ON TABLE info_slug TO api_write;

-- migrate:down
DROP TABLE info_slug;
