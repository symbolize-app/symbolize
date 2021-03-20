-- migrate:up
CREATE TABLE taxon (
        id
                UUID DEFAULT gen_random_uuid() PRIMARY KEY,
        created
                TIMESTAMPTZ(3) NOT NULL DEFAULT current_timestamp(3),
        updated
                TIMESTAMPTZ(3) NOT NULL DEFAULT current_timestamp(3),
        deleted
                TIMESTAMPTZ(3) NULL,
        member_id
                UUID NOT NULL REFERENCES member (id),
        parent_taxon_id
                UUID NULL REFERENCES taxon (id),
        names
                JSONB NOT NULL,
        slug
                TEXT NOT NULL,
        intro
                TEXT NOT NULL
);

-- migrate:down
DROP TABLE taxon;
