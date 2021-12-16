SELECT
  topic.id,
  topic.updated_at,
  topic.title,
  topic.slug,
  topic.content
FROM
  topic
WHERE
  topic.deleted = false
ORDER BY
  topic.created_at DESC
LIMIT
  5;
