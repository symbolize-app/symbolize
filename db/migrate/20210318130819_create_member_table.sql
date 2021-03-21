-- migrate:up
CREATE TABLE member (
        id
                BYTES PRIMARY KEY,
        created
                TIMESTAMPTZ(0) NOT NULL DEFAULT current_timestamp(0),
        deleted
                TIMESTAMPTZ(0) NULL,
        email
                STRING NOT NULL UNIQUE,
        handle
                STRING NOT NULL UNIQUE
);

-- migrate:down
DROP TABLE member;
