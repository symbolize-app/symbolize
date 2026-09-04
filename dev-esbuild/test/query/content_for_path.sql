select contents.original
from content as contents
inner join path as paths on contents.id = paths.content_id
where paths.id = ?
order by paths.version_id desc
limit 1;
