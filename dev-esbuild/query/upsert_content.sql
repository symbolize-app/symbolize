insert into content (id, original) values (
  cast(:id as blob), cast(:original as blob)
) on conflict do nothing; -- noqa: PRS
