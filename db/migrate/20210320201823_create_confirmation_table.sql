-- migrate:up
CREATE TABLE confirmation (
        id
                BYTES PRIMARY KEY,
        created
                TIMESTAMPTZ(0) NOT NULL DEFAULT current_timestamp(0),
        deleted
                TIMESTAMPTZ(0) NULL,
        member_id
                BYTES NOT NULL REFERENCES member (id)
);

-- migrate:down
DROP TABLE confirmation;
