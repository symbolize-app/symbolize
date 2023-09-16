-- $1 buffer_seconds
-- $2 limit
-- $3 language
-- $4 updated_at
-- $5 type
-- $6 id
WITH
  document
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
        jsonb_build_array() AS parents,
        topic.title,
        jsonb_build_array() AS names,
        topic.slug,
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
          reply.subforum_id,
          reply.topic_id,
          NULL AS taxon_rank,
          jsonb_build_array() AS parents,
          NULL AS title,
          jsonb_build_array() AS names,
          reply.slug,
          reply.tags,
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
          taxon.slug,
          jsonb_build_array() AS tags,
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
          jsonb_build_array() AS parents,
          info.title,
          jsonb_build_array() AS names,
          info.slug,
          info.tags,
          info.content
        FROM
          info
    )
SELECT
  document.type,
  document.id,
  document.created_at,
  document.created_by,
  document.updated_at,
  document.deleted,
  document.subforum_id,
  document.topic_id,
  document.taxon_rank,
  document.parents,
  document.title,
  document.names,
  document.slug,
  document.tags,
  document.content
FROM
  document
WHERE
  document.language = $3
  AND document.updated_at
    < current_timestamp(0)
      - '1 second'::INTERVAL * $1::FLOAT8
  AND (
      $4::TIMESTAMPTZ(0) IS NULL
      OR document.updated_at > $4::TIMESTAMPTZ(0)
      OR document.updated_at = $4::TIMESTAMPTZ(0)
        AND (
            document.type > $5::STRING
            OR document.type = $5::STRING
              AND document.id > $6::BYTES
          )
    )
ORDER BY
  document.updated_at, document.type, document.id
LIMIT
  $2::INT8;
