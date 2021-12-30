import * as appApiTest from '@fe/api/index.test.node.ts'
import * as timeNode from '@tiny/core/time.node.ts'
import * as urlModule from 'node:url'

if (
  process.argv[1] ===
  urlModule.fileURLToPath(import.meta.url)
) {
  void appApiTest
    .run({
      ...timeNode.initContext(),
    })
    .then((success) => {
      process.exit(success ? 0 : 1)
    })
}
