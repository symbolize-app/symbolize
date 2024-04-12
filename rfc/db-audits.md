# DB audits

To track changes to relational data, store previous versions of rows with some light event metadata.

- Each table with mutable rows will get a paired audits table
- Writes to the audits table will happen automatically via a trigger
  - Rusqlite allows Rust triggers
- If there's some special event associated with a latest change, special event columns will also be set
  - To be unset later on the next change
- In the audits column, there's a nullable copy of each original column, plus a "changed" boolean column
- Changes are stored backward, so they can be applied one by one to the current version to get a previous version
- Large strings can be compressed and/or diff encoded
  - Can use imara-diff in Rust (the diff engine for gitoxide)
- Schema migrations are done to both the original table and the audits table
