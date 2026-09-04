// The semaphore state machine is Gleam-owned. This is the one runtime seam
// needed to retain the source Promise/microtask observation boundary.
export function schedule(callback) {
  globalThis.queueMicrotask(callback)
}
