insert into path (id, version_id, content_id) values (
  cast(:id as text),
  cast(:version_id as integer),
  cast(:content_id as blob)
) on conflict do nothing; -- noqa: PRS
