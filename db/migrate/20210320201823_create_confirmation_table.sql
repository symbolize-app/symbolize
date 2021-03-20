-- migrate:up
CREATE TABLE confirmation (
        token_hash
                BYTES PRIMARY KEY,
        created
                TIMESTAMPTZ(3) NOT NULL DEFAULT current_timestamp(3),
        deleted
                TIMESTAMPTZ(3) NULL,
        member_id
                UUID NOT NULL REFERENCES member (id)
);

-- migrate:down
DROP TABLE confirmation;