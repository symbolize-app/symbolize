SELECT
  id AS id, title AS title, slug AS slug, content AS content
FROM
  topic
WHERE
  topic.deleted IS NULL
ORDER BY
  topic.created DESC
LIMIT
  5;
