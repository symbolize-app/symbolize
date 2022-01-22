import * as appApiTest from '@app/api/index.test.node.ts'
import * as tinyTime from '@tiny/core/time.node.ts'
import * as nodeUrl from 'node:url'

if (
  process.argv[1] === nodeUrl.fileURLToPath(import.meta.url)
) {
  void appApiTest
    .run({
      ...tinyTime.initContext(),
    })
    .then((success) => {
      process.exit(success ? 0 : 1)
    })
}
