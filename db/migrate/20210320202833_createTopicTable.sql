-- migrate:up
CREATE TABLE topic (
  id
    BYTES PRIMARY KEY,
  created
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  updated
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  deleted
    TIMESTAMPTZ(0) NULL,
  member_id
    BYTES NOT NULL REFERENCES member (id),
  title
    STRING NOT NULL,
  slug
    STRING NOT NULL,
  content
    STRING NOT NULL
);
GRANT SELECT ON TABLE topic TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE topic TO api_write;

-- migrate:down
DROP TABLE topic;
