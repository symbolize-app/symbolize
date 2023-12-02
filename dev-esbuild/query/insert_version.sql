insert into version (id) values (
  cast(:id as integer)
) on conflict do nothing; -- noqa: PRS
