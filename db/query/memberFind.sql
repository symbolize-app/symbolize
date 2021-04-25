-- $1 id
SELECT email AS email FROM member WHERE member.id = $1;
