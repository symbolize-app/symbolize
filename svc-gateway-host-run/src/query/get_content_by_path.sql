select
  cast(content.id as blob) as id,
  cast(content.original as blob) as original
from content, path
where
  path.id = cast(:path_id as text)
  and path.version_id = (select max(version.id) from version)
  and content.id = path.content_id;
