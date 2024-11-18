import type * as streamContext from '@/context.ts'

class StreamImpl implements streamContext.Stream {
  // eslint-disable-next-line @typescript-eslint/require-await -- just throw
  async fetch(): Promise<Response> {
    throw new Error('Unexpected request in test')
  }
}

export function stream(): streamContext.Stream {
  return new StreamImpl()
}
