-- migrate:up
CREATE TABLE confirmation (
  id
    BYTES PRIMARY KEY,
  created
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  deleted
    TIMESTAMPTZ(0) NULL,
  member_id
    BYTES NOT NULL REFERENCES member (id)
);
GRANT SELECT ON TABLE confirmation TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE confirmation TO api_write;

-- migrate:down
DROP TABLE confirmation;
