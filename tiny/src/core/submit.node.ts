import * as submit from '@tiny/core/submit.ts'
import * as formData from 'formdata-polyfill/esm.min.js'
import fetch from 'node-fetch'

export function initContext(): submit.Context {
  /* eslint-disable @typescript-eslint/naming-convention */
  return submit.initContext({
    fetch: (fetch as unknown) as typeof window.fetch,
    FormData: formData.FormData,
  })
  /* eslint-enable @typescript-eslint/naming-convention */
}
