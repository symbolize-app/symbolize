export interface ConnectionRequest {
  readonly clientStream: ReadableStream<unknown>
  readonly connectionId: number
  readonly service: string
  readonly type: 'ConnectionRequest'
}

export interface ConnectionResponse {
  readonly connectionId: number
  readonly serverStream: ReadableStream<unknown>
  readonly type: 'ConnectionResponse'
}
