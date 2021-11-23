-- $1 updated_at
-- $2 type
-- $3 id
WITH
  data
    AS (
      SELECT
        'topic' AS type,
        topic.id,
        topic.created_at,
        topic.created_by,
        topic.updated_at,
        topic.deleted,
        topic.language,
        topic.subforum_id,
        NULL AS topic_id,
        NULL AS taxon_rank,
        topic.title,
        NULL AS names,
        topic.tags,
        topic.content
      FROM
        topic
      UNION ALL
        SELECT
          'reply' AS type,
          reply.id,
          reply.created_at,
          reply.created_by,
          reply.updated_at,
          reply.deleted,
          reply.language,
          NULL AS subforum_id,
          reply.topic_id,
          NULL AS taxon_rank,
          NULL AS title,
          NULL AS names,
          NULL AS tags,
          reply.content
        FROM
          reply
      UNION ALL
        SELECT
          'taxon' AS type,
          taxon.id,
          taxon.created_at,
          taxon.created_by,
          taxon.updated_at,
          taxon.deleted,
          taxon.language,
          NULL AS subforum_id,
          NULL AS topic_id,
          taxon.taxon_rank,
          NULL AS title,
          taxon.names,
          NULL AS tags,
          taxon.content
        FROM
          taxon
      UNION ALL
        SELECT
          'info' AS type,
          info.id,
          info.created_at,
          info.created_by,
          info.updated_at,
          info.deleted,
          info.language,
          NULL AS subforum_id,
          NULL AS topic_id,
          NULL AS taxon_rank,
          info.title,
          NULL AS names,
          info.tags,
          info.content
        FROM
          info
    )
SELECT
  data.language
FROM
  data
WHERE
  $1::TIMESTAMPTZ(0) IS NULL
  OR data.updated_at > $1::TIMESTAMPTZ(0)
  OR data.updated_at = $1::TIMESTAMPTZ(0)
    AND (
        $2::STRING IS NULL
        OR data.type > $2::STRING
        OR data.type = $2::STRING
          AND ($3::BYTES IS NULL OR data.id > $3::BYTES)
      )
ORDER BY
  data.updated_at, data.type, data.id
LIMIT
  10;
