-- migrate:up
CREATE TABLE session (
        token_hash
                BYTES PRIMARY KEY,
        created
                TIMESTAMPTZ(3) NOT NULL DEFAULT current_timestamp(3),
        deleted
                TIMESTAMPTZ(3) NULL,
        browser
                TEXT NOT NULL,
        os
                TEXT NOT NULL,
        member_id
                UUID NOT NULL REFERENCES member (id),
        last_activity
                TIMESTAMPTZ NOT NULL,
        last_country
                TIMESTAMPTZ NOT NULL,
        last_language
                TIMESTAMPTZ NOT NULL
);

-- migrate:down
DROP TABLE session;
