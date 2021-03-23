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
GRANT SELECT, INSERT, UPDATE ON TABLE confirmation TO api;

-- migrate:down
DROP TABLE confirmation;
