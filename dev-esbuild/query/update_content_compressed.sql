update content set compressed = cast(:compressed as blob)
where content.id = cast(:id as blob);
