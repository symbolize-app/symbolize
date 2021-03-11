import * as apiTest from '@fe/api/index.test.ts'

export function main(): void {
  if (process.send) {
    process.send('ready')
  }

  void apiTest.run()
}
