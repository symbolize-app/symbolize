-- migrate:up
CREATE ROLE IF NOT EXISTS 'api';
ALTER ROLE 'api' WITH LOGIN;

-- migrate:down
ALTER ROLE 'api' WITH NOLOGIN;
