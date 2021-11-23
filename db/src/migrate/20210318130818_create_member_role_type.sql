-- migrate:up
CREATE TYPE member_role AS ENUM ('admin', 'normal');

-- migrate:down
DROP TYPE member_role;
