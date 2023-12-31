export type ConnectionRequest = {
  type: 'ConnectionRequest'
  connectionId: number
  service: string
  clientStream: ReadableStream<unknown>
}

export type ConnectionResponse = {
  type: 'ConnectionResponse'
  connectionId: number
  serverStream: ReadableStream<unknown>
}
