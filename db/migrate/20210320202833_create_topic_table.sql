-- migrate:up
CREATE TABLE topic (
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
        title
                TEXT NOT NULL,
        slug
                TEXT NOT NULL,
        content
                TEXT NOT NULL
);

-- migrate:down
DROP TABLE topic;
