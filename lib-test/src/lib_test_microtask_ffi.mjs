export function queue_microtask(callback) {
  globalThis.queueMicrotask(callback)
}

export function raise(value) {
  throw value
}
