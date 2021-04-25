-- $1 id
-- $2 member_id
-- $3 title
-- $4 slug
-- $5 content
WITH
  topic_insert
    AS (
      INSERT
      INTO
        topic (id, member_id, title, slug, content)
      VALUES
        ($1, $2, $3, $4, $5)
      ON CONFLICT
        (id)
      DO
        NOTHING
      RETURNING
        id, slug
    )
INSERT
INTO
  topic_slug (slug, topic_id)
SELECT
  topic_insert.slug, topic_insert.id
FROM
  topic_insert;
