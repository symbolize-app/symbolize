# Reaction system

Of course fun reactions are nice to have, but the system itself can be built to emphasize community support and meaningful contributions.

## Tech

- Store in same row as a the message
- Use hash map: `{ author_id: [reaction_1, reaction_2] }`
- If max count reached, they spill out into thread replies
  - Don’t block, just trigger new shards (use this for all shards)

## Notifications

Nice to have, because it shows people are reading your stuff.

## Methods

- Quote reply
- Reply in time (tell server the last message you saw)
- Start thread (tell server you’re intending to start a thread)
- Explicit reaction UI

For the normal text methods, there's a default threshold that includes emoji and one-word replies like OK, no, thanks. This can be configured per-site, subforum, blog, etc..

## Display

- Display random set, jumble effect (random size, offset, orientation)
- Hover or select to trigger loop animation (clipped vertical scroll for all reactions?)
