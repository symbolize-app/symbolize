-- migrate:up
CREATE TYPE language AS ENUM ('en', 'fr', 'ja');

-- migrate:down
DROP TYPE language;
