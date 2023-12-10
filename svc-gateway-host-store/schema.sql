create table if not exists migration (version varchar(128) primary key);
create table version (
  id integer primary key not null
) strict, without rowid;
create table content (
  id blob primary key not null,
  original blob not null,
  compressed blob null
) strict;
create table path (
  id text not null,
  version_id integer not null,
  content_id blob not null,
  primary key (id, version_id),
  foreign key (version_id) references version (id),
  foreign key (content_id) references content (id)
) strict;
-- Dbmate schema migrations
insert into migration (version) values
('20231203011147'),
('20231203011148'),
('20231204175648'),
('20231204175649');
