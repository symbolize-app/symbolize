export interface ConnectionRequest {
  clientStream: ReadableStream<unknown>
  connectionId: number
  service: string
  type: 'ConnectionRequest'
}

export interface ConnectionResponse {
  connectionId: number
  serverStream: ReadableStream<unknown>
  type: 'ConnectionResponse'
}
