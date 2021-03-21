-- migrate:up
CREATE TABLE session (
        id
                BYTES PRIMARY KEY,
        created
                TIMESTAMPTZ(0) NOT NULL DEFAULT current_timestamp(0),
        deleted
                TIMESTAMPTZ(0) NULL,
        member_id
                BYTES NOT NULL REFERENCES member (id),
        browser
                STRING NOT NULL,
        os
                STRING NOT NULL,
        countries
                JSONB NOT NULL,
        languages
                JSONB NOT NULL,
        last_activity
                TIMESTAMPTZ NOT NULL
);

-- migrate:down
DROP TABLE session;
