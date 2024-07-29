# Event paths

All services in the system communicate using events. This is the isolation boundary, and a mechanism for working with real-time information.

Each service has a restricted scope for improved reliability and maintainability.

- Host services run on trusted infrastructure
- Guest services run on untrusted browsers
- Read services don't make data changes
- Write services do make data changes
- View services render parts of the webpage

# Host-guest

Guest services use HTTP/2 streams to communicate with host services.

The `gateway-guest-run` service establishes the connection using the Fetch API from a dedicated service worker. The `gateway-host-run` service handles the request using the `hyper` library.

Because browsers only support "half duplex" communication for one `fetch` request, the browser must make two requests. One for sending discrete, async events, and one for receiving them. This way, host (and guest) services can work on async tasks and send async updates.

A pair of requests will be sent for every host-guest link required. By using the same origin, a single HTTP/2 connection can be shared (avoiding extra TCP/TLS handshakes). But by having separate streams, each stream can own its own backpressure signal, and each host service has an easy, direct link to send events back to the guest service. This also introduces some failure isolation, where a single host-guest link can fail without interrupting others.

One complication of using two requests is that there is no guarantee from the browser that these requests will share a single HTTP/2 connection. If they do not share a connection, they may not reach the same gateway host service instance. To guard against this, the gateway host instance will refuse response streams when a matching request stream is not found, and terminate request streams without matching response streams.

This correlation can happen using a guest ID, which can also be used for session tracking, and sending async events to the gateway guest service instance.

# Guest-guest

Within the browser, services communicate with each other using the Streams API. For one tab, all read and write services run on one dedicated worker and all view services run on one window.

To bridge the gap between a service worker and a window, the service worker (server) listens for a browser-level message. The window (client) posts a message event that includes a `ReadableStream` that the server will use to listen for future application-level events. The server then responds by posting back a browser-level message that includes another `ReadableStream`, this time for the client to listen to future application-level events.

Two `ReadableStream` objects are also used for communication between any other service pairs (both in the service worker or both in the window). For this case, browser-level message passing is not used for coordination.

`ReadableStream` was used over simple messages or emitters because they support backpressure and assembling into pipelines.

# Host-host

Each host service runs in its own process and listens for HTTP/2 requests on its own port. In local development, all processes run in a shared environment, but in production they will be in isolated environments.

To establish a connection between host services, the client service makes an HTTP/2 request using `hyper` to the server. Because `hyper` supports full duplex, only one request is needed.

For client requests forwarded by the host gateway to a specific host service, the host gateway will still generate new requests to the other host service, but will share an existing HTTP/2 connection is avaialble.

# Alternatives to HTTP/2 streams

## Server-sent events

Implemented in browsers as early as 2010 via a long-running request where JSON objects are pulled out and surfaced to the JS event listeners.

Main drawbacks compared to HTTP/2 streams:

- no backpressure
- forced JSON usage
- unidirectional

## WebSockets

Implemented in browsers also as early as 2010 via a separate TCP/TLS connection that bypasses the HTTP stack after an initial upgrade handshake.

Main drawbacks compared to HTTP/2 streams:

- no backpressure
- unreliable disconnect detection
- difficult to process upgrade handshake due to HTTP mismatches
- complicated heartbeat management

## WebTransport

This API requires an HTTP/3 connection, which runs on QUIC, a network protocol built on UDP.

A main benefit for async communication is this removes the TCP problem of head-of-line blocking, where a single failed TCP packet will block the entire TCP connection.

With WebTransport, each stream runs independently. Unreliable messages can even be sent, when loss is tolerable.

Main advantages compared to HTTP/2 streams:

- cheap stream creation (not a full request)
- no head-of-line blocking
- full duplex
- option for unreliable datagrams
- connection migration during network changes

As of 2024, more browsers are getting HTTP/3 and WebTransport support, and even the Rust `hyper` library has support. The main blocker is optionality to use Fly for host infrastructure, where QUIC support is still missing from Fly Proxy.
