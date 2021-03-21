-- migrate:up
CREATE TABLE topic (
        id
                BYTES PRIMARY KEY,
        created
                TIMESTAMPTZ(0) NOT NULL DEFAULT current_timestamp(0),
        updated
                TIMESTAMPTZ(0) NOT NULL DEFAULT current_timestamp(0),
        deleted
                TIMESTAMPTZ(0) NULL,
        member_id
                BYTES NOT NULL REFERENCES member (id),
        title
                STRING NOT NULL,
        slug
                STRING NOT NULL,
        content
                STRING NOT NULL
);

-- migrate:down
DROP TABLE topic;
