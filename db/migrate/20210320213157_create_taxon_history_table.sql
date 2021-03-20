-- migrate:up
CREATE TABLE taxon_history (
        taxon_id
                UUID NOT NULL REFERENCES taxon (id),
        updated
                TIMESTAMPTZ(3) NOT NULL,
        member_id
                UUID NOT NULL REFERENCES member (id),
        parent_taxon_id
                UUID NULL REFERENCES taxon (id),
        names
                JSONB NOT NULL,
        slug
                TEXT NOT NULL,
        intro
                TEXT NOT NULL,
        PRIMARY KEY (taxon_id, updated)
);

-- migrate:down
DROP TABLE taxon_history;
