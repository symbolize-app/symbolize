# Authentication

Main authentication method is magic links. They're very easy to use and very secure by default.

Opening a link from email will open a webpage just for processing the link -- it will not automatically close or redirect to the app.

## Education

Never seen it on a passworded or passwordless site, but from the very first signup, start educating authors on the risks of phishing and manipulator-in-the-middle. Train them to use the hints in their browser and in the email to verify authenticity.

Using and storing session location & IP address provides important security benefits. Opt-out is not possible, but authors with privacy concerns are highly encouraged to use location-masking services.

## Enumeration

To prevent an enumeration attack ("querying" which emails are registered)...

- Combine signup and login into one form
  - Use one mechanism client-side to prevent distinguishing
  - Use similar method server-side for simplicity
- Rate limit by IP address
- Require captcha pass
- Use fixed-length response time, regardless of code path

## Display

Show key information in the original session, the magic link email, and the magic link webpage:

- Location & IP
- Domain name
- Browser
- Language
- Local time
- Session title

The session title is a random sequence of regular words from a fixed dictionary. It's actually an encoding of the 128-bit session ID, using 16 words from a dictionary with size 512 (designed for uniqueness).

(These data points will be used to identify the session in the active sessions list.)

Of these, only location & IP is non-trivial for a MitM to forge. But the others are difficult for a phisher to forge.

Additionally, in the magic link email and webpage, also show a list of recent account (authentication) activity.

## Session pinning

Sessions are pinned to various attributes. This simplifies session listing, and reduces risk of session hijacking.

To mitigate the UX effects (e.g. an author who frequently changes time zones or countries or VPN proxies), the web client can store multiple session tokens without terminating any.

## Magic link algorithm

- Only valid for 5 minutes
- Can only be used once
  - Single DB transaction invalidates the link and authenticates the original session
  - If there's a network failure after transaction commit and magic link client doesn't get a response...
    - Resubmission will fail (expired/unknown link)
    - Original session will be valid
