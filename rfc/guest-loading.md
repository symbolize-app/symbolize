# Guest loading

Guest code will be run at three levels:

- Service worker
- Shared worker
- Window

Manifest schema:

- Version
- Path
- Content

Content API:

- Get by content ID and file extension
- Get latest version by path

## Service worker

### Initial install flow

- Initial page loaded without any main guest code
- Click a button to install the service worker

### Upgrade flow

- Service worker code loads together with embedded manifest JSON
  - Manifest version ID
  - All paths and content IDs
- Block on human intervention if unsafe
  - Loaded pages, recent activity, unsaved changes, release mode, etc.
- Skip waiting
- Send message to windows to reload
  - New windows will load a new version-specific shared worker
  - Old shared worker will expire with no windows left
- Prepare cache
  - Delayed, after upgrade finished, to be responsive to actual critical path
  - Remove all extras from cache
  - Fetch any missing into cache
  - One cache, shared across service worker versions
  - Creates race condition with other service worker versions, but OK since cache eviction is assumed possible

### Fetch intercepts

- Intercept code requests, originally would be fetching by path, instead fetch content ID according to current service worker manifest
- Wait for in-flight promise if exists
- Should be a cache hit
  - If miss, rexamine cache and start up all missing promises

## Code splitting

### Service worker

- Service worker code and manifest should be split
- Code should be quite stable, isolated from manifest changes
- Manifest should also be split by service and dependency, again for isolation
- Splits loaded via hash-specific `importScripts`
  - This top level script is generated
- Service worker code needs to be bundled and can't use ES Modules
  - Not supported, difficult semantics
- Practical isolation benefits from splitting seem weak, actual benefit is likely more from caching
  - Big chunks of unchanged splits don't need re-downloading

### Shared worker and window

- Split with ES modules
- Hardcoded top level has module with all async imports
  - One import for each service
- On initialization, each service is required to negotiate its own stream communication
- Also use async imports inside services where possible
  - Helps avoid overloading the JS engine on page load

## Code deduplication

- Once duplicated event definitions are present in multiple services...
- Service worker can create stub "export all from import by content ID" JS files
- Used for all JS files, for simplicity, benefit is in the dups

## Dev reload

- As files are compiled, they can be pushed to a live client
  - Just needs caching by content ID and file extension
- When compilation is done (and successful), a reload is triggered
- Some/all of the files may be ready to use with the new manifest
- The pushes will need to be handled by the shared worker, since the service worker doesn't handle long-running connections
