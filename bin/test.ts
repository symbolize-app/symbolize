import * as apiTest from '@fe/api/index.test.node.ts'
import * as timeNode from '@tiny/util/time.node.ts'
import * as urlModule from 'url'

if (
  process.argv[1] ===
  urlModule.fileURLToPath(import.meta.url)
) {
  void apiTest
    .run({
      ...timeNode.initContext(),
    })
    .then((success) => {
      process.exit(success ? 0 : 1)
    })
}
