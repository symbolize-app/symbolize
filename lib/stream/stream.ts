export type Context = Pick<
  typeof window,
  'ReadableStream' | 'TransformStream' | 'WritableStream'
>
