-- migrate:up
CREATE TABLE taxon_slug (
        slug
                STRING PRIMARY KEY,
        created
                TIMESTAMPTZ(0) NOT NULL DEFAULT current_timestamp(0),
        taxon_id
                BYTES NOT NULL REFERENCES taxon (id)
);

-- migrate:down
DROP TABLE taxon_slug;
