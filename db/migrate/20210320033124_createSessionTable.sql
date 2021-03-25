-- migrate:up
CREATE TABLE session (
  id
    BYTES PRIMARY KEY,
  created
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  deleted
    TIMESTAMPTZ(0) NULL,
  member_id
    BYTES NOT NULL REFERENCES member (id),
  browser
    STRING NOT NULL,
  os
    STRING NOT NULL,
  countries
    JSONB NOT NULL,
  languages
    JSONB NOT NULL,
  last_activity
    TIMESTAMPTZ NOT NULL
);
GRANT SELECT ON TABLE session TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE session TO api_write;

-- migrate:down
DROP TABLE session;
