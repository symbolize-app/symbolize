import * as submit from '@tiny/core/submit.ts'
import * as nodeFetch from 'node-fetch'

export function initContext(): submit.Context {
  return submit.initContext({
    fetch:
      nodeFetch.default as unknown as typeof window.fetch,
  })
}
