SELECT
  topic.id AS id,
  topic.updated AS updated,
  topic.title AS title,
  topic.slug AS slug,
  topic.content AS content
FROM
  topic
WHERE
  topic.deleted IS NULL
ORDER BY
  topic.created DESC
LIMIT
  5;
