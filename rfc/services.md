# Services

Split code into separate library-based services for change isolation. Ideally, this separation will allow faster iteration due to few interdependencies.

All services will run together in one process if possible. Interfaces must be clearly defined, and links between services should be kept to a minimum (and avoid cycles).

## Scope

Each service has a restricted scope for improved reliability and maintainability.

- Host services run on trusted infrastructure
- Guest services run on untrusted browsers
- Read services don't make data changes
- Write services do make data changes
- View services render parts of the webpage

## Data

Each service owns read/write access to its own DB shards. This data design will largely determine the boundaries and links between services.

## Queues

To provide fair access to service resources, all inter-service requests will go through the service's main request queue, even requests originating in the same process.

## Code duplication

Don't try to always factor out code that's duplicated between services.

There will be case where utility libraries are the best option, but the benefit of deduplicated code needs to be weighed against the loss of flexibility and agility for individual services, and the introduction of a shared source of bugs.

## Resources

Until benchmarked and proven "better", heterogeneous resources will also remain in the main process:

- Media processing
- Full-text search
- Server-side rendering

## Errors

Failing fast and providing a descriptive error message is important in the case when a dependent service responds with an error. It should be expected that other services might fail due to for example resource contention or software bugs.

There may be some situations where a reasonable fallback or degradation is possible, but the investment and upkeep required would need a very strong rationale.

## Alternatives to library-based services

Using one process but no libraries removes the need for interface definition, and has a high risk for entropy.

Using network-distributed processes complicates development considerably and introduces baseline latency and unreliability.
