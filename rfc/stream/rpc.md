# Stream RPC

All communication between services is modelled as remote procedure calls. These calls are implemented via request and response messages using the various stream paths.

## Versioning

The target service defines the full interface and there is no API metadata or version negotiation.

Requests and responses are simple JSON, so some basic extensions are allowed without breaking the JSON schema. But if a breaking change is required for typing or semantic reasons, the version of the call will be bumped.

Each call has its own version. It's expected that a service will support new and old versions of the call during upgrades. If possible, internally it will translate old requests into new requests and new responses back to old responses.

## Deadlines

When making a call, the origin service will know its own time budget for what is an acceptable response time. If the target service exceeds that time, the origin service will cancel the request.

When both services are running in the same context, the cancellation can be communicated via a flag in memory. But for requests sent of an HTTP stream, the only reliable signal will be to close the stream, dropping the request along with any other in progress or queued requests.

The target service should periodically check if a request has been cancelled to avoid wasting resources: especially before starting processing, and before making any nested calls.

## Service level objectives

While calls will have deadlines, services themselves will have response time service level objectives. If the response time exceeds the target, the service will force the response to be an error. The goal with being strict here is to raise latency issues early as major failures, starting at the moment they begin happening.

When making a nested call, a service will set the deadline of the call based on its own time remaining for the current request according to its response time target.

Due to the way services use each other, there may be a hierarchy with different tiers of response time objectives.

## Errors

All errors will be opaque. Any messages will be for internal purposes, and will not be translated or presented in the web interface.

If a response needs "exceptional" cases, it will use a sum type with all of the exact information needed for each case. This includes common cases like security-related responses.

This way, all errors will be assumed to be either transient and/or bugs. Either way they can all be handled the same way by the origin service.

## Retries

All requests must be idempotent. If a call still has deadline remaining and a response error is received, the call will retry with exponential backoff and jitter.

## Streaming responses

The response type of some calls will be an asynchronous stream of items. This will be modelled in JavaScript as an async iterable, and in Rust as a futures stream.

Streams can be cancelled by the origin service and can end with an error or success from the target service. In JavaScript, cancelling will be an explicit method, and in Rust it will be happen automatically when the stream is dropped.

At a lower level, streaming responses will work as special messages on the response stream (items, errors, completion) and request stream (cancellation). All of these messages are essentially requests, with deadlines, retries, etc.

All processing of response streams should be done by an origin service in an async background task. Then even though new items technically arrive on the response stream of the origin service, they trigger new processing and thus will re-enqueue on the request queue of the origin service before processing begins.

## Nested backpressure

While individual requests will respond to backpressue due to queueing enforced by the target service, any backpressure in nested calls will not immediately signal backpressure "up the chain".

This is because nested calls will be modelled as normal code, not as a dataflow graph where downstream dependencies are statically known.

Instead, backpressure will manifest through slow processing of requests which will eventually cause the service's queue to reach capacity.

## Alternatives to explicit requests/responses

Some systems opt to use async events with low coupling between producer and consumer services. This is fundamentally incompatible with fine-grained backpressure because it there is no direct link between a producer and a consumer. Any slow consumer would need to slow down all producers. (Fine-grained backpressure is required for this project to maintain strong latency targets.)
