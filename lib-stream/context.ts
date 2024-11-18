export interface Context {
  readonly stream: Stream
}

export interface Stream {
  readonly fetch: (typeof globalThis)['fetch']
}

class StreamImpl implements Stream {
  readonly fetch = globalThis.fetch.bind(globalThis)
}

export function stream(): Stream {
  return new StreamImpl()
}
