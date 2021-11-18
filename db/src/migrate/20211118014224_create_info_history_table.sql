-- migrate:up
CREATE TABLE info_history (
  info_id
    BYTES NOT NULL REFERENCES info (id),
  updated_at
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  updated_by
    BYTES NULL REFERENCES member (id),
  deleted
    BOOL NOT NULL,
  published
    BOOL NOT NULL,
  language
    language NOT NULL,
  cross_language_id
    BYTES NOT NULL,
  title
    STRING NOT NULL,
  slug
    STRING NOT NULL,
  tags
    JSONB NOT NULL,
  content
    STRING NOT NULL,
  PRIMARY KEY (info_id, updated_at)
);
GRANT SELECT ON TABLE info_history TO api_read;
GRANT SELECT, INSERT ON TABLE info_history TO api_write;

-- migrate:down
DROP TABLE info_history;
