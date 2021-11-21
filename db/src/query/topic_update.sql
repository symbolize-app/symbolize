-- $1 id
-- $2 updated_at_old
-- $3 title
-- $4 slug
-- $5 content
WITH
  topic_update
    AS (
      UPDATE
        topic
      SET
        updated
          = CASE
          WHEN (topic.title, topic.slug, topic.content)
          = ($3, $4, $5)
          THEN topic.updated
          WHEN topic.updated >= current_timestamp(0)
          THEN NULL
          ELSE current_timestamp(0)
          END,
        title = $3,
        slug = $4,
        content = $5
      FROM
        topic AS topic_old
      WHERE
        topic.id = $1
        AND (
            topic.updated = $2
            OR (topic.title, topic.slug, topic.content)
              = ($3, $4, $5)
          )
        AND topic_old.id = topic.id
      RETURNING
        topic.id,
        topic.updated,
        topic.slug,
        topic_old.updated AS updated_old,
        topic_old.title AS title_old,
        topic_old.slug AS slug_old,
        topic_old.content AS content_old
    ),
  topic_history_insert
    AS (
      INSERT
      INTO
        topic_history
          (topic_id, updated, title, slug, content)
      SELECT
        topic_update.id,
        topic_update.updated_old,
        topic_update.title_old,
        topic_update.slug_old,
        topic_update.content_old
      FROM
        topic_update
      WHERE
        topic_update.updated > topic_update.updated_old
      RETURNING
        topic_history.updated AS updated
    ),
  topic_slug_insert
    AS (
      INSERT
      INTO
        topic_slug (slug, topic_id)
      SELECT
        topic_update.slug, topic_update.id
      FROM
        topic_update
      WHERE
        topic_update.slug != topic_update.slug_old
      RETURNING
        topic_slug.slug AS slug
    )
SELECT
  topic_update.updated AS updated
FROM
  topic_update;
