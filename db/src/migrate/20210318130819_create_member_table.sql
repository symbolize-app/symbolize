-- migrate:up
CREATE TABLE member (
  id
    BYTES PRIMARY KEY,
  created_at
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  updated_at
    TIMESTAMPTZ(0) DEFAULT current_timestamp(0) NOT NULL,
  updated_by
    BYTES NULL REFERENCES member (id),
  delete_scheduled_at
    TIMESTAMPTZ(0) DEFAULT NULL NULL,
  member_role
    member_role DEFAULT 'normal' NOT NULL,
  private
    BOOL DEFAULT false NOT NULL,
  email
    STRING NOT NULL UNIQUE,
  handle
    STRING NOT NULL UNIQUE
);
GRANT SELECT ON TABLE member TO api_read;
GRANT SELECT, INSERT, UPDATE ON TABLE member TO api_write;

-- migrate:down
DROP TABLE member;
