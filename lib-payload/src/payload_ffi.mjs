export class PayloadError extends Error {
  constructor(message) {
    super(message)
    this.name = 'PayloadError'
  }
}

export function raise_payload_error(message) {
  throw new PayloadError(message)
}

export function call_and_catch(callback) {
  try {
    callback()
    return ''
  } catch (error) {
    return `${error?.name ?? 'Error'}:${error?.message ?? String(error)}`
  }
}
