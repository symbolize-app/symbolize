-- migrate:up
CREATE TABLE member (
  id
    BYTES PRIMARY KEY,
  created
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  deleted
    TIMESTAMPTZ(0) NULL,
  email
    STRING NOT NULL UNIQUE,
  handle
    STRING NOT NULL UNIQUE
);
GRANT SELECT, INSERT, UPDATE ON TABLE member TO api;

-- migrate:down
DROP TABLE member;
