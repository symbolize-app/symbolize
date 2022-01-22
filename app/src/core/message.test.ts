import * as appMessage from '@fe/core/message.ts'
import * as tinyTest from '@tiny/test/index.ts'

export const url = import.meta.url

export const tests = {
  ['hi message'](): void {
    tinyTest.assertEquals(appMessage.hi, 'Hello')
  },
}
