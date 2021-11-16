-- $1 id
SELECT
  member.email AS email
FROM
  member
WHERE
  member.id = $1;
