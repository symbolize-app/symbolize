-- migrate:up
CREATE TABLE member (
        id
                UUID DEFAULT gen_random_uuid() PRIMARY KEY,
        created
                TIMESTAMPTZ(3) NOT NULL DEFAULT current_timestamp(3),
        deleted
                TIMESTAMPTZ(3) NULL,
        email
                TEXT NOT NULL UNIQUE,
        handle
                TEXT NOT NULL UNIQUE
);

-- migrate:down
DROP TABLE member;
