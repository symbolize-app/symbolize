select count(*) as row_count
from content
where content.compressed is not null;
