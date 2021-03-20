-- migrate:up
CREATE TABLE reply_history (
        reply_topic_id
                UUID NOT NULL,
        reply_created
                TIMESTAMPTZ(3) NOT NULL,
        updated
                TIMESTAMPTZ(3) NOT NULL,
        content
                TEXT NOT NULL,
        PRIMARY KEY (reply_topic_id, reply_created, updated),
        FOREIGN KEY (reply_topic_id, reply_created) REFERENCES reply (topic_id, created)

);

-- migrate:down
DROP TABLE reply_history;
