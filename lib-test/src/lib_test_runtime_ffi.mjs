export function set_timeout(callback, milliseconds) {
  globalThis.setTimeout(callback, milliseconds)
}

export function call_and_catch(callback) {
  try {
    callback()
    return 'no error'
  } catch (error) {
    return String(error?.message ?? error)
  }
}
