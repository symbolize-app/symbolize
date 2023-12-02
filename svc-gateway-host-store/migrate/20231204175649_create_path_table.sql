-- migrate:up

create table path (
  id text not null,
  version_id integer not null,
  content_id blob not null,
  primary key (id, version_id),
  foreign key (version_id) references version (id),
  foreign key (content_id) references content (id)
) strict;

-- migrate:down

drop table path;
