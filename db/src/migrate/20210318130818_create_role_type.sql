-- migrate:up
CREATE TYPE role AS ENUM ('admin', 'normal');

-- migrate:down
DROP TYPE role;
