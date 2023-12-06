-- migrate:up

create table content (
  id blob primary key not null,
  original blob not null,
  compressed blob null
) strict;

-- migrate:down

drop table content;
