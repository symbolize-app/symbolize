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
        topic.published,
        topic.language,
        topic.subforum_id,
        NULL AS topic_id,
        NULL AS rank,
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
          reply.published,
          reply.language,
          NULL AS subforum_id,
          reply.topic_id,
          NULL AS rank,
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
          taxon.published,
          taxon.language,
          NULL AS subforum_id,
          NULL AS topic_id,
          taxon.rank,
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
          info.published,
          info.language,
          NULL AS subforum_id,
          NULL AS topic_id,
          NULL AS rank,
          info.title,
          NULL AS names,
          info.tags,
          info.content
        FROM
          info
    )
SELECT
  updated_at, type, id::STRING
FROM
  data
WHERE
  data.updated_at > '2021-11-18 00:00:00+00:00'
  OR data.updated_at = '2021-11-18 00:00:00+00:00'
    AND (
        data.type > 'reply'
        OR data.type = 'reply' AND data.id > b'\x003037'
      )
ORDER BY
  data.updated_at, data.type, data.id
LIMIT
  10;
