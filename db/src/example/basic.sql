INSERT
INTO
  member
    (
      id,
      created_at,
      updated_at,
      updated_by,
      member_role,
      email,
      handle
    )
VALUES
  (
    e'\\x00000000000000000000000000000000000000000000000000000000000000aa',
    '2021-11-17 00:00:00+00:00',
    '2021-11-18 00:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000aa',
    'admin',
    'aa@example.org',
    'aax'
  )
ON CONFLICT
  (id)
DO
  NOTHING;

INSERT
INTO
  member
    (id, created_at, updated_at, updated_by, email, handle)
VALUES
  (
    e'\\x00000000000000000000000000000000000000000000000000000000000000bb',
    '2021-11-18 00:00:00+00:00',
    '2021-11-18 00:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000bb',
    'bb@example.org',
    'bbx'
  )
ON CONFLICT
  (id)
DO
  NOTHING;

INSERT
INTO
  member_history
    (
      id,
      updated_at,
      updated_by,
      member_role,
      private,
      delete_scheduled_at,
      email,
      handle
    )
SELECT
  member.id,
  member.created_at,
  member.id,
  member.member_role,
  member.private,
  member.delete_scheduled_at,
  member.email,
  'aaz'
FROM
  member
WHERE
  member.id
  = e'\\x00000000000000000000000000000000000000000000000000000000000000aa'
ON CONFLICT
  (id, updated_at)
DO
  NOTHING;

INSERT
INTO
  session
    (
      id,
      created_at,
      last_active_at,
      member_id,
      browser,
      os,
      countries,
      languages,
      recent_activity
    )
VALUES
  (
    e'\\x0000000000000000000000000000000000000000000000000000000000000000',
    '2021-11-17 00:00:00+00:00',
    '2021-11-18 00:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000aa',
    'Chrome',
    'Linux',
    jsonb_build_array('Canada'),
    jsonb_build_array('en'),
    jsonb_build_array(
      '2021-11-17 12:00:00+00:00',
      '2021-11-18 00:00:00+00:00'
    )
  )
ON CONFLICT
  (id)
DO
  NOTHING;

INSERT
INTO
  one_time_login (id, created_at, member_id, session_id)
VALUES
  (
    e'\\x0000000000000000000000000000000000000000000000000000000000000001',
    '2021-11-17 00:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000aa',
    e'\\x0000000000000000000000000000000000000000000000000000000000000000'
  )
ON CONFLICT
  (id)
DO
  NOTHING;

-- Log in using http://localhost:8080/?one-time-login=ab
INSERT
INTO
  one_time_login (id, member_id)
VALUES
  (
    e'\\x00000000000000000000000000000000000000000000000000000000000000ab',
    e'\\x00000000000000000000000000000000000000000000000000000000000000aa'
  )
ON CONFLICT
  (id)
DO
  NOTHING;

INSERT
INTO
  subforum
    (
      id,
      created_at,
      created_by,
      updated_at,
      updated_by,
      language,
      cross_language_id,
      name,
      slug,
      description
    )
VALUES
  (
    e'\\x0000000000000000000000000000000000000000000000000000000000000002',
    '2021-11-17 00:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000aa',
    '2021-11-18 00:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000aa',
    'en',
    e'\\x0000000000000000000000000000000000000000000000000000000000000003',
    'Random',
    'random',
    'The Random Forum'
  )
ON CONFLICT
  (id)
DO
  NOTHING;

INSERT
INTO
  subforum_slug (id, language, slug)
SELECT
  subforum.id, subforum.language, subforum.slug
FROM
  subforum
WHERE
  subforum.id
  = e'\\x0000000000000000000000000000000000000000000000000000000000000002'
ON CONFLICT
  (language, slug)
DO
  NOTHING;

INSERT
INTO
  subforum_history
    (
      id,
      updated_at,
      updated_by,
      deleted,
      cross_language_id,
      name,
      slug,
      description
    )
SELECT
  subforum.id,
  subforum.created_at,
  subforum.created_by,
  subforum.deleted,
  subforum.cross_language_id,
  '????',
  subforum.slug,
  subforum.description
FROM
  subforum
WHERE
  subforum.id
  = e'\\x0000000000000000000000000000000000000000000000000000000000000002'
ON CONFLICT
  (id, updated_at)
DO
  NOTHING;

INSERT
INTO
  tag
    (
      id,
      created_at,
      created_by,
      updated_at,
      updated_by,
      language,
      cross_language_id,
      name,
      description
    )
VALUES
  (
    e'\\x0000000000000000000000000000000000000000000000000000000000000004',
    '2021-11-17 00:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000aa',
    '2021-11-18 00:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000aa',
    'en',
    e'\\x0000000000000000000000000000000000000000000000000000000000000005',
    'aster',
    'The Aster plant family '
  )
ON CONFLICT
  (id)
DO
  NOTHING;

INSERT
INTO
  tag_history
    (
      id,
      updated_at,
      updated_by,
      deleted,
      cross_language_id,
      name,
      description
    )
SELECT
  tag.id,
  tag.created_at,
  tag.created_by,
  tag.deleted,
  tag.cross_language_id,
  'sunflower',
  tag.description
FROM
  tag
WHERE
  tag.id
  = e'\\x0000000000000000000000000000000000000000000000000000000000000004'
ON CONFLICT
  (id, updated_at)
DO
  NOTHING;

INSERT
INTO
  topic
    (
      id,
      created_at,
      created_by,
      updated_at,
      updated_by,
      bumped_at,
      bumped_by,
      deleted,
      language,
      subforum_id,
      title,
      slug,
      tags,
      content
    )
VALUES
  (
    e'\\x0000000000000000000000000000000000000000000000000000000000000006',
    '2021-11-17 00:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000aa',
    '2021-11-18 00:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000aa',
    '2021-11-19 00:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000aa',
    false,
    'en',
    e'\\x0000000000000000000000000000000000000000000000000000000000000002',
    'Hello',
    'hello',
    jsonb_build_array(
      '0000000000000000000000000000000000000000000000000000000000000004'
    ),
    'Hi there'
  )
ON CONFLICT
  (id)
DO
  NOTHING;

INSERT
INTO
  topic_slug (id, language, slug)
SELECT
  topic.id, topic.language, topic.slug
FROM
  topic
WHERE
  topic.id
  = e'\\x0000000000000000000000000000000000000000000000000000000000000006'
ON CONFLICT
  (language, slug)
DO
  NOTHING;

INSERT
INTO
  topic_history
    (
      id,
      saved_at,
      saved_by,
      scheduled_at,
      updated_at,
      deleted,
      subforum_id,
      title,
      slug,
      tags,
      content
    )
SELECT
  topic.id,
  topic.created_at,
  topic.created_by,
  topic.updated_at,
  topic.updated_at,
  topic.deleted,
  topic.subforum_id,
  topic.title,
  topic.slug,
  topic.tags,
  '...'
FROM
  topic
WHERE
  topic.id
  = e'\\x0000000000000000000000000000000000000000000000000000000000000006'
ON CONFLICT
  (id, saved_at)
DO
  NOTHING;

INSERT
INTO
  topic_bump (id, created_at, created_by)
SELECT
  topic.id, topic.bumped_at, topic.bumped_by
FROM
  topic
WHERE
  topic.id
  = e'\\x0000000000000000000000000000000000000000000000000000000000000006'
ON CONFLICT
  (id, created_at)
DO
  NOTHING;

INSERT
INTO
  reply
    (
      id,
      created_at,
      created_by,
      updated_at,
      updated_by,
      deleted,
      language,
      subforum_id,
      topic_id,
      slug,
      tags,
      content
    )
VALUES
  (
    e'\\x0000000000000000000000000000000000000000000000000000000000000007',
    '2021-11-17 00:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000bb',
    '2021-11-18 00:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000bb',
    false,
    'en',
    e'\\x0000000000000000000000000000000000000000000000000000000000000002',
    e'\\x0000000000000000000000000000000000000000000000000000000000000006',
    'hello',
    jsonb_build_array(
      '0000000000000000000000000000000000000000000000000000000000000004'
    ),
    'Yo'
  )
ON CONFLICT
  (id)
DO
  NOTHING;

INSERT
INTO
  reply_history
    (
      id,
      saved_at,
      saved_by,
      scheduled_at,
      updated_at,
      deleted,
      content
    )
SELECT
  reply.id,
  reply.created_at,
  reply.created_by,
  reply.created_at,
  reply.created_at,
  reply.deleted,
  reply.content
FROM
  reply
WHERE
  reply.id
  = e'\\x0000000000000000000000000000000000000000000000000000000000000007'
ON CONFLICT
  (id, saved_at)
DO
  NOTHING;

INSERT
INTO
  reply
    (
      id,
      created_at,
      created_by,
      updated_at,
      updated_by,
      deleted,
      language,
      subforum_id,
      topic_id,
      slug,
      tags,
      content
    )
VALUES
  (
    e'\\x0000000000000000000000000000000000000000000000000000000000000008',
    '2021-11-18 01:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000bb',
    '2021-11-18 01:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000bb',
    false,
    'en',
    e'\\x0000000000000000000000000000000000000000000000000000000000000002',
    e'\\x0000000000000000000000000000000000000000000000000000000000000006',
    'hello',
    jsonb_build_array(
      '0000000000000000000000000000000000000000000000000000000000000004'
    ),
    'Test 2'
  )
ON CONFLICT
  (id)
DO
  NOTHING;

INSERT
INTO
  reply
    (
      id,
      created_at,
      created_by,
      updated_at,
      updated_by,
      deleted,
      language,
      subforum_id,
      topic_id,
      parent_reply_id,
      slug,
      tags,
      content
    )
VALUES
  (
    e'\\x0000000000000000000000000000000000000000000000000000000000000009',
    '2021-11-18 02:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000bb',
    '2021-11-18 02:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000bb',
    false,
    'en',
    e'\\x0000000000000000000000000000000000000000000000000000000000000002',
    e'\\x0000000000000000000000000000000000000000000000000000000000000006',
    e'\\x0000000000000000000000000000000000000000000000000000000000000007',
    'hello',
    jsonb_build_array(
      '0000000000000000000000000000000000000000000000000000000000000004'
    ),
    'Test 3'
  )
ON CONFLICT
  (id)
DO
  NOTHING;

INSERT
INTO
  taxon
    (
      id,
      created_at,
      created_by,
      updated_at,
      updated_by,
      deleted,
      language,
      cross_language_id,
      taxon_rank,
      parents,
      names,
      slug,
      content
    )
VALUES
  (
    e'\\x000000000000000000000000000000000000000000000000000000000000000a',
    '2021-11-17 00:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000aa',
    '2021-11-18 00:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000aa',
    false,
    'en',
    e'\\x000000000000000000000000000000000000000000000000000000000000000b',
    'family',
    jsonb_build_array(),
    jsonb_build_array('flower', 'fleur'),
    'flower',
    'Number of petals'
  )
ON CONFLICT
  (id)
DO
  NOTHING;

INSERT
INTO
  taxon_slug (id, language, slug)
SELECT
  taxon.id, taxon.language, taxon.slug
FROM
  taxon
WHERE
  taxon.id
  = e'\\x000000000000000000000000000000000000000000000000000000000000000a'
ON CONFLICT
  (language, slug)
DO
  NOTHING;

INSERT
INTO
  taxon_history
    (
      id,
      saved_at,
      saved_by,
      scheduled_at,
      updated_at,
      deleted,
      cross_language_id,
      taxon_rank,
      parents,
      names,
      slug,
      content
    )
SELECT
  taxon.id,
  taxon.created_at,
  taxon.created_by,
  taxon.created_at,
  taxon.created_at,
  taxon.deleted,
  taxon.cross_language_id,
  taxon.taxon_rank,
  taxon.parents,
  jsonb_build_array('flower'),
  taxon.slug,
  taxon.content
FROM
  taxon
WHERE
  taxon.id
  = e'\\x000000000000000000000000000000000000000000000000000000000000000a'
ON CONFLICT
  (id, saved_at)
DO
  NOTHING;

INSERT
INTO
  taxon
    (
      id,
      created_at,
      created_by,
      updated_at,
      updated_by,
      deleted,
      language,
      cross_language_id,
      taxon_rank,
      parents,
      names,
      slug,
      content
    )
VALUES
  (
    e'\\x000000000000000000000000000000000000000000000000000000000000000d',
    '2021-11-18 00:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000aa',
    '2021-11-18 00:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000aa',
    false,
    'en',
    e'\\x000000000000000000000000000000000000000000000000000000000000000b',
    'species',
    jsonb_build_array(
      '000000000000000000000000000000000000000000000000000000000000000a'
    ),
    jsonb_build_array('sunflower'),
    'sunflower',
    'Faces the sun'
  )
ON CONFLICT
  (id)
DO
  NOTHING;

INSERT
INTO
  taxon_slug (id, language, slug)
SELECT
  taxon.id, taxon.language, taxon.slug
FROM
  taxon
WHERE
  taxon.id
  = e'\\x000000000000000000000000000000000000000000000000000000000000000d'
ON CONFLICT
  (language, slug)
DO
  NOTHING;

INSERT
INTO
  info
    (
      id,
      created_at,
      created_by,
      updated_at,
      updated_by,
      deleted,
      language,
      cross_language_id,
      title,
      slug,
      tags,
      content
    )
VALUES
  (
    e'\\x000000000000000000000000000000000000000000000000000000000000000c',
    '2021-11-17 00:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000aa',
    '2021-11-18 00:00:00+00:00',
    e'\\x00000000000000000000000000000000000000000000000000000000000000aa',
    false,
    'en',
    e'\\x000000000000000000000000000000000000000000000000000000000000000d',
    'Water',
    'water',
    jsonb_build_array(
      '0000000000000000000000000000000000000000000000000000000000000004'
    ),
    'Check if wet or dry'
  )
ON CONFLICT
  (id)
DO
  NOTHING;

INSERT
INTO
  info_slug (id, language, slug)
SELECT
  info.id, info.language, info.slug
FROM
  info
WHERE
  info.id
  = e'\\x000000000000000000000000000000000000000000000000000000000000000c'
ON CONFLICT
  (language, slug)
DO
  NOTHING;

INSERT
INTO
  info_history
    (
      id,
      saved_at,
      saved_by,
      scheduled_at,
      updated_at,
      deleted,
      cross_language_id,
      title,
      slug,
      tags,
      content
    )
SELECT
  info.id,
  info.created_at,
  info.created_by,
  info.created_at,
  info.created_at,
  info.deleted,
  info.cross_language_id,
  'W',
  info.slug,
  info.tags,
  info.content
FROM
  info
WHERE
  info.id
  = e'\\x000000000000000000000000000000000000000000000000000000000000000c'
ON CONFLICT
  (id, saved_at)
DO
  NOTHING;
