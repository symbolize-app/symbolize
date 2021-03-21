-- migrate:up
CREATE TYPE rank AS ENUM ('kingdom', 'family', 'genus', 'species', 'variant');

-- migrate:down
DROP TYPE rank;
