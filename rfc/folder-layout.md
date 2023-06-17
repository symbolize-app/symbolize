# Folder layout

Like biology achieves scale through hierarchy and repetition.

## Tasks

## Top level

- build
- service
- lib
- task

## Services

Put in `/service`.

- auth (RBAC, groups, logins, alts)
- blog (personal, community)
- book (long form, chapters, glossary, index)
- cal (physica/virtual meetups, in-site events)
- chat (private, group)
- connect (integrations, ActivityPub, Git, Intertwine)
- feed (content subscriptions)
- forum (long-form communication, public, private)
- frame (header, footer, nav)
- gateway* (event forwarder, service worker)
- letter (newsletters)
- log (dev logging)
- media (file, photo, video, audio, animated image, console session, screen recording)
- meet (live calls, 1:1, group, party)
- mod (moderation queues, spam filters)
- obj (structured data)
- ping (notifications)
- profile (name, signature, image)
- render* (SSR)
- repo (change management, source control)
- review (change review)
- search* (full-text search)
- site (registration, theme, billing)
- task (project management)
- track (internal/external analytics)
- vote (open votes, straw polls)

## Standard

- doc
- guest
  - db
  - display
  - read
  - write
- host
  - db
  - read
  - write
- stream

## Special*

Some of the above.

## Libraries

Put in `/lib`.

- content (schema, review, scheduling, blocks)
  - ts
- db
  - rs
  - ts
- http
  - rs
  - ts
- stream
  - rs
  - ts
- style
  - ts
- widget
  - ts
