# Folder layout

## Services

- auth (RBAC, groups, logins, alts)
- blog (personal, community)
- book (long form, chapters, glossary, index)
- cal (physica/virtual meetups, in-site events)
- chat (private, group)
- connect (integrations, ActivityPub, Git, Intertwine)
- feed (content subscriptions)
- forum (long-form communication, public, private)
- frame (header, footer, nav)
- gateway\* (event forwarder, service worker)
- letter (newsletters)
- log (dev logging)
- media (file, photo, video, audio, animated image, console session, screen recording)
- meet (live calls, 1:1, group, party)
- mod (moderation queues, spam filters)
- obj (structured data)
- ping (notifications)
- profile (name, signature, image)
- render\* (SSR)
- repo (change management, source control)
- review (change review)
- search\* (full-text search)
- site (registration, theme, billing)
- task (project management)
- track (internal/external analytics)
- vote (open votes, straw polls)

### Standard

Each service has a restricted scope for improved reliability and maintainability.

- Host services run on trusted infrastructure
- Guest services run on untrusted browsers
- Read services don't make data changes
- Write services do make data changes
- View services render parts of the webpage

This results in the following folders per service:

- svc-<service>-doc
- svc-<service>-guest-read
- svc-<service>-guest-store
- svc-<service>-guest-view
- svc-<service>-guest-write
- svc-<service>-host-read
- svc-<service>-host-store
- svc-<service>-host-write

### Special\*

Read/write switch to...

- svc-<service>-guest-run
- svc-<service>-host-run

## Misc

- dev-\*
- rfc

## Libraries

- lib-content (schema, review, scheduling, blocks)
- lib-convey
- lib-http-rs
- lib-http-ts
- lib-store-rs
- lib-store-ts
- lib-stream-rs
- lib-stream-ts
