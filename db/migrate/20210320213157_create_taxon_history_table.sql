-- migrate:up
CREATE TABLE taxon_history (
        taxon_id
                BYTES NOT NULL REFERENCES taxon (id),
        updated
                TIMESTAMPTZ(0) NOT NULL,
        member_id
                BYTES NOT NULL REFERENCES member (id),
        parent_taxon_id
                BYTES NULL REFERENCES taxon (id),
        rank
                rank NOT NULL,
        names
                JSONB NOT NULL,
        slug
                STRING NOT NULL,
        intro
                STRING NOT NULL,
        PRIMARY KEY (taxon_id, updated)
);

-- migrate:down
DROP TABLE taxon_history;
