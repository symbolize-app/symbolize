INSERT
INTO
  member
    (
      id,
      created_at,
      updated_at,
      updated_by,
      "role",
      email,
      handle
    )
VALUES
  (
    b'\xaa',
    '2021-11-17 00:00:00+00:00',
    '2021-11-18 00:00:00+00:00',
    b'\xaa',
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
    b'\xbb',
    '2021-11-18 00:00:00+00:00',
    '2021-11-18 00:00:00+00:00',
    b'\xbb',
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
      member_id,
      updated_at,
      updated_by,
      "role",
      private,
      delete_scheduled,
      email,
      handle
    )
SELECT
  member.id,
  member.created_at,
  member.id,
  member.role,
  member.private,
  member.delete_scheduled,
  member.email,
  'aaz'
FROM
  member
WHERE
  member.id = b'\xaa'
ON CONFLICT
  (member_id, updated_at)
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
    b'\x0000',
    '2021-11-17 00:00:00+00:00',
    '2021-11-18 00:00:00+00:00',
    b'\xaa',
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
    b'\x0001',
    '2021-11-17 00:00:00+00:00',
    b'\xaa',
    b'\x0000'
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
  (b'\xab', b'\xaa')
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
    b'\x0002',
    '2021-11-17 00:00:00+00:00',
    b'\xaa',
    '2021-11-18 00:00:00+00:00',
    b'\xaa',
    'en',
    b'\x0003',
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
  subforum_slug (language, slug, subforum_id)
SELECT
  subforum.language, subforum.slug, subforum.id
FROM
  subforum
WHERE
  subforum.id = b'\x0002'
ON CONFLICT
  (language, slug)
DO
  NOTHING;

INSERT
INTO
  subforum_history
    (
      subforum_id,
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
  subforum.id = b'\x0002'
ON CONFLICT
  (subforum_id, updated_at)
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
    b'\x0004',
    '2021-11-17 00:00:00+00:00',
    b'\xaa',
    '2021-11-18 00:00:00+00:00',
    b'\xaa',
    'en',
    b'\x0005',
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
      tag_id,
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
  tag.id = b'\x0004'
ON CONFLICT
  (tag_id, updated_at)
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
      published,
      language,
      subforum_id,
      title,
      slug,
      tags,
      content
    )
VALUES
  (
    b'\x0006',
    '2021-11-17 00:00:00+00:00',
    b'\xaa',
    '2021-11-18 00:00:00+00:00',
    b'\xaa',
    true,
    'en',
    b'\x0002',
    'Hello',
    'hello',
    jsonb_build_array(e'\\x0004'),
    'Hi there'
  )
ON CONFLICT
  (id)
DO
  NOTHING;

INSERT
INTO
  topic_slug (language, slug, topic_id)
SELECT
  topic.language, topic.slug, topic.id
FROM
  topic
WHERE
  topic.id = b'\x0006'
ON CONFLICT
  (language, slug)
DO
  NOTHING;

INSERT
INTO
  topic_history
    (
      topic_id,
      updated_at,
      updated_by,
      deleted,
      published,
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
  topic.deleted,
  topic.published,
  topic.subforum_id,
  topic.title,
  topic.slug,
  topic.tags,
  '...'
FROM
  topic
WHERE
  topic.id = b'\x0006'
ON CONFLICT
  (topic_id, updated_at)
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
      published,
      language,
      topic_id,
      content
    )
VALUES
  (
    b'\x0007',
    '2021-11-17 00:00:00+00:00',
    b'\xbb',
    '2021-11-18 00:00:00+00:00',
    b'\xbb',
    true,
    'en',
    b'\x0006',
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
      reply_id,
      updated_at,
      updated_by,
      deleted,
      published,
      content
    )
SELECT
  reply.id,
  reply.created_at,
  reply.created_by,
  reply.deleted,
  false,
  reply.content
FROM
  reply
WHERE
  reply.id = b'\x0007'
ON CONFLICT
  (reply_id, updated_at)
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
      published,
      language,
      topic_id,
      content
    )
VALUES
  (
    b'\x0008',
    '2021-11-18 01:00:00+00:00',
    b'\xbb',
    '2021-11-18 01:00:00+00:00',
    b'\xbb',
    true,
    'en',
    b'\x0006',
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
      published,
      language,
      topic_id,
      parent_reply_id,
      content
    )
VALUES
  (
    b'\x0009',
    '2021-11-18 02:00:00+00:00',
    b'\xbb',
    '2021-11-18 02:00:00+00:00',
    b'\xbb',
    true,
    'en',
    b'\x0006',
    b'\x0007',
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
      published,
      language,
      cross_language_id,
      rank,
      parent_taxon_id,
      names,
      slug,
      content
    )
VALUES
  (
    b'\x000a',
    '2021-11-17 00:00:00+00:00',
    b'\xaa',
    '2021-11-18 00:00:00+00:00',
    b'\xaa',
    true,
    'en',
    b'\x000b',
    'family',
    NULL,
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
  taxon_slug (language, slug, taxon_id)
SELECT
  taxon.language, taxon.slug, taxon.id
FROM
  taxon
WHERE
  taxon.id = b'\x000a'
ON CONFLICT
  (language, slug)
DO
  NOTHING;

INSERT
INTO
  taxon_history
    (
      taxon_id,
      updated_at,
      updated_by,
      deleted,
      published,
      cross_language_id,
      rank,
      parent_taxon_id,
      names,
      slug,
      content
    )
SELECT
  taxon.id,
  taxon.created_at,
  taxon.created_by,
  taxon.deleted,
  taxon.published,
  taxon.cross_language_id,
  taxon.rank,
  taxon.parent_taxon_id,
  jsonb_build_array('flower'),
  taxon.slug,
  taxon.content
FROM
  taxon
WHERE
  taxon.id = b'\x000a'
ON CONFLICT
  (taxon_id, updated_at)
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
      published,
      language,
      cross_language_id,
      title,
      slug,
      tags,
      content
    )
VALUES
  (
    b'\x000c',
    '2021-11-17 00:00:00+00:00',
    b'\xaa',
    '2021-11-18 00:00:00+00:00',
    b'\xaa',
    true,
    'en',
    b'\x000d',
    'Water',
    'water',
    jsonb_build_array(e'\\x0004'),
    'Check if wet or dry'
  )
ON CONFLICT
  (id)
DO
  NOTHING;

INSERT
INTO
  info_slug (language, slug, info_id)
SELECT
  info.language, info.slug, info.id
FROM
  info
WHERE
  info.id = b'\x000c'
ON CONFLICT
  (language, slug)
DO
  NOTHING;

INSERT
INTO
  info_history
    (
      info_id,
      updated_at,
      updated_by,
      deleted,
      published,
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
  info.deleted,
  info.published,
  info.cross_language_id,
  'W',
  info.slug,
  info.tags,
  info.content
FROM
  info
WHERE
  info.id = b'\x000c'
ON CONFLICT
  (info_id, updated_at)
DO
  NOTHING;
