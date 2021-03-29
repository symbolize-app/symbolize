import * as message from '@fe/core/message.ts'
import * as test from '@tiny/test/index.ts'

export const url = import.meta.url

export const tests = {
  ['hi message'](): void {
    test.assertEquals(message.hi, 'Hello')
  },
}
