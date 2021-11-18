-- migrate:up
CREATE TABLE info_slug (
  slug
    STRING PRIMARY KEY,
  created
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  info_id
    BYTES NOT NULL REFERENCES info (id)
);
GRANT SELECT ON TABLE info_slug TO api_read;
GRANT SELECT, INSERT ON TABLE info_slug TO api_write;

-- migrate:down
DROP TABLE info_slug;
