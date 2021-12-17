import * as submit from '@tiny/core/submit.ts'
import fetch from 'node-fetch'
import { FormData } from 'formdata-polyfill/esm.min.js'

export function initContext(): submit.Context {
  return submit.initContext({
    fetch: fetch as typeof window.fetch,
    FormData,
  })
}
