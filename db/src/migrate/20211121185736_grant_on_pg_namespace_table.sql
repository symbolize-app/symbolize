-- migrate:up
-- See https://github.com/cockroachdb/cockroach/issues/49313#issuecomment-974879849
GRANT CONNECT ON DATABASE defaultdb TO api_read;
GRANT CONNECT ON DATABASE defaultdb TO api_write;

-- migrate:down
REVOKE CONNECT ON DATABASE defaultdb FROM api_read;
REVOKE CONNECT ON DATABASE defaultdb FROM api_write;
