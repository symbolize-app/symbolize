# Services

Split code into separate library-based services for change isolation. Ideally, this separation will allow faster iteration due to few interdependencies.

All services will run together in one process. Interfaces must be clearly defined, and links between services should be kept to a minimum (and avoid cycles).

## Queues

## Errors

## Storage

## Code duplication

## Resources

Until benchmarked and proven "better", heterogeneous resources like will also remain in the main process:

- Media processing
- Full-text search 
- Server-side rendering

## Alternatives to library-based services

Using one process but no libraries removes the need for interface definition, and has a high risk for entropy.

Using network-distributed processes complicates development considerably and introduces baseline latency and unreliability.
