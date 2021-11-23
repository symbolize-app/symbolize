-- migrate:up
CREATE TYPE taxon_rank AS ENUM ('kingdom', 'family', 'genus', 'species', 'variant');

-- migrate:down
DROP TYPE taxon_rank;
