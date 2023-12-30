# Backpressure

To avoid overloading services, each service needs its own bounded queue, work-in-progress limiter, and backpressure signal.

If a service tries to send data to another service but gets a backpressure signal, it needs to stop sending. It can try again after an exponentially-increasing random delay.

# JavaScript

JavaScript uses streams, and depending on the type of object you're working with, there are safe, backpressure-aware operations you can use.

Each stream has its own internal queue and own high water mark. Once the queue reaches the high water mark, backpressure begins. The high water mark can be defined in terms of item count, byte length, or any custom measure.

## `ReadableStream`

- Implement source that only enqueues chunks for the controller when pull is called
  - Will work toward goal of keeping internal queue filled to high water mark
- Pipe through a `TransformStream`
- Pipe to a `WritableStream`
- Lock and get a `ReadableStreamDefaultReader`

## `WritableStream`

- Implement a sink that only resolves its returned write promise when ready for the next write
  - Will ensure backpressure signal begins when queue of unwritten chunks reaches high water mark
- Pipe from a `ReadableStream`
- Lock and get a `WritableStreamDefaultWriter`

## `TransformStream`

- Implement a transformer that only resolves its returned transform promise when ready for the next transform
  - Will ensure backpressure signal begins when queue of untransformed chunks reaches high water mark
- Exposes both ends its ends as `ReadableStream` and `WritableStream`
- Actually has one queue for each contained (readable and writable) stream
- Pipe through via a `ReadableStream`

## `ReadableStreamDefaultReader`

- Read when ready to use next chunk

## `WritableStreamDefaultWriter`

- Write when ready to provide next chunk
- Always check ready promise before writing
  - Ensures not writing past high water mark

# Buffer tuning

- Start with small/reasonable size queues
- Monitor with real world data
- Avoid exceptionally huge queues
  - This will lead to "buffer bloat" -- high latency as queues drain, when they should have errored out (propagated back pressure) instead of accepting more data

# Scaling

- If a service can scale up to deal with backpressure, make sure that backpressure doesn't start until at least when the scale up signal starts
  - This avoids the service stuck scaled down and "failing" with backpressure
