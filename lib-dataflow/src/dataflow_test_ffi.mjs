export function schedule_delay(milliseconds, callback) {
  globalThis.setTimeout(callback, milliseconds)
}
