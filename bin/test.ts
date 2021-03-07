import * as apiTest from '@fe/api/index.t.ts'
import * as urlModule from 'url'

if (
  process.argv[1] ===
  urlModule.fileURLToPath(import.meta.url)
) {
  void apiTest.run().then((success) => {
    process.exit(success ? 0 : 1)
  })
}
