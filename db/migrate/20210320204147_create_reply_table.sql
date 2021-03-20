-- migrate:up
CREATE TABLE reply (
        topic_id
                UUID NOT NULL REFERENCES topic (id),
        created
                TIMESTAMPTZ(3) NOT NULL DEFAULT current_timestamp(3),
        deleted
                TIMESTAMPTZ(3) NULL,
        member_id
                UUID NOT NULL REFERENCES member (id),
        PRIMARY KEY (topic_id, created)
);

-- migrate:down
DROP TABLE reply;
