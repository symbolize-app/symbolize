-- $1 buffer_seconds
-- $2 limit
-- $3 language
-- $4 updated_at
-- $5 type
-- $6 id
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
        topic.id AS topic_id,
        NULL AS taxon_rank,
        NULL AS parents,
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
          NULL AS parents,
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
          taxon.parents,
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
          NULL AS parents,
          info.title,
          NULL AS names,
          info.tags,
          info.content
        FROM
          info
    )
SELECT
  *
FROM
  data
WHERE
  data.language = $3
  AND data.updated_at
    < current_timestamp(0) - '1 second'::INTERVAL * $1::INT8
  AND (
      $4::TIMESTAMPTZ(0) IS NULL
      OR data.updated_at > $4::TIMESTAMPTZ(0)
      OR data.updated_at = $4::TIMESTAMPTZ(0)
        AND (
            data.type > $5::STRING
            OR data.type = $5::STRING
              AND data.id > $6::BYTES
          )
    )
ORDER BY
  data.updated_at, data.type, data.id
LIMIT
  $2::INT8;
