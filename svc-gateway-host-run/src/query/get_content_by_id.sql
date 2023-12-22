select cast(content.original as blob) from content where
  content.id = cast(:content_id as blob)
union all
select cast(null as blob);
