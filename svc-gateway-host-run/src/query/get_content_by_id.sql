select cast(content.original as blob) as original from content where
  content.id = cast(:content_id as blob);
