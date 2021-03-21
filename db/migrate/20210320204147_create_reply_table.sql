-- migrate:up
CREATE TABLE reply (
        topic_id
                BYTES NOT NULL REFERENCES topic (id),
        id
                BYTES,
        created
                TIMESTAMPTZ(0) NOT NULL DEFAULT current_timestamp(0),
        updated
                TIMESTAMPTZ(0) NOT NULL DEFAULT current_timestamp(0),
        deleted
                TIMESTAMPTZ(0) NULL,
        member_id
                BYTES NOT NULL REFERENCES member (id),
        parent_reply_id
                BYTES NULL,
        content
                STRING NOT NULL,
        PRIMARY KEY (topic_id, id),
        FOREIGN KEY (topic_id, parent_reply_id) REFERENCES reply (topic_id, id)
);

-- migrate:down
DROP TABLE reply;
