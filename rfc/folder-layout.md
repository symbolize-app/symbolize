# Folder layout

Like biology achieves scale through hierarchy and repetition.

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

- svc-<service>-doc
- svc-<service>-guest-read
- svc-<service>-guest-store
- svc-<service>-guest-view
- svc-<service>-guest-write
- svc-<service>-host-read
- svc-<service>-host-store
- svc-<service>-host-write

### Special\*

Some of the above.

## Misc

- dev-\*
- rfc

## Libraries

- lib-content-guest (schema, review, scheduling, blocks)
- lib-store-guest
- lib-store-host
- lib-http-guest
- lib-http-host
- lib-stream-guest
- lib-stream-host
- lib-style-guest
- lib-widget-guest
