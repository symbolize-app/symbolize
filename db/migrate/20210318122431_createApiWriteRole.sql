-- migrate:up
CREATE ROLE IF NOT EXISTS 'api_write';
ALTER ROLE 'api_write' WITH LOGIN;

-- migrate:down
