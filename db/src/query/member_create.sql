-- $1 id
-- $2 email
-- $3 handle
INSERT
INTO
  member (id, email, handle)
VALUES
  ($1, $2, $3)
ON CONFLICT
  (id)
DO
  NOTHING;
