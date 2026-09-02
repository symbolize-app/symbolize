export function queue_microtask(callback) {
  globalThis.queueMicrotask(callback)
}
