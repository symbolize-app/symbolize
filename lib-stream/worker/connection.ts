export interface WorkerConnectionRequest {
  readonly clientStream: ReadableStream<string>
  readonly connectionId: number
  readonly service: string
  readonly type: 'WorkerConnectionRequest'
}

export interface WorkerConnectionResponse {
  readonly connectionId: number
  readonly serverStream: ReadableStream<string>
  readonly type: 'WorkerConnectionResponse'
}
