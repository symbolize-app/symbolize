-- migrate:up
CREATE ROLE IF NOT EXISTS 'api_read';
ALTER ROLE 'api_read' WITH LOGIN;

-- migrate:down
