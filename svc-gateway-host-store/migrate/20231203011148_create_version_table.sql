-- migrate:up

create table version (
  id integer primary key not null
) strict, without rowid;

-- migrate:down

drop table version;
