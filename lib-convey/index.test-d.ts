/* eslint-disable @typescript-eslint/no-empty-function */
/* eslint-disable @typescript-eslint/ban-ts-comment */
import * as convey from '@/index.ts'

export const url = import.meta.url

export const tests = {
  ['div no readonly attrs'](): void {
    convey.html.div({
      // @ts-expect-error
      childElementCount: 1,
    })
    convey.html.div({
      // @ts-expect-error
      // eslint-disable-next-line @typescript-eslint/naming-convention
      ATTRIBUTE_NODE: 1,
    })
  },

  ['div no complex attrs'](): void {
    convey.html.div({
      // @ts-expect-error
      onclick() {},
    })
  },
}
