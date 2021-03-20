-- migrate:up
CREATE TABLE session (
        token_hash
                BYTES PRIMARY KEY,
        created
                TIMESTAMPTZ(3) NOT NULL DEFAULT current_timestamp(3),
        deleted
                TIMESTAMPTZ(3) NULL,
        member_id
                UUID NOT NULL REFERENCES member (id),
        browser
                TEXT NOT NULL,
        os
                TEXT NOT NULL,
        countries
                JSONB NOT NULL,
        languages
                JSONB NOT NULL,
        last_activity
                TIMESTAMPTZ NOT NULL
);

-- migrate:down
DROP TABLE session;
