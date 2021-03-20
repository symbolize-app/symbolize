-- migrate:up
CREATE TABLE taxon_slug (
        slug
                TEXT PRIMARY KEY,
        created
                TIMESTAMPTZ(3) NOT NULL DEFAULT current_timestamp(3),
        taxon_id
                UUID NOT NULL REFERENCES taxon (id)
);

-- migrate:down
DROP TABLE taxon_slug;
