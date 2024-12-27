## DB blobs

Blobs will be stored as separate immutable pages in the object store. Any retrieval will be performed by the write leader, and saved locally in an LRU cache (that gets distributed to followers).
