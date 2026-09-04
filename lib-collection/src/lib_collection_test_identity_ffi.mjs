export function new_object() {
  return {}
}

export function new_nan() {
  return NaN
}

export function new_negative_zero() {
  return -0
}

export function new_positive_zero() {
  return 0
}

export function call_and_catch(callback) {
  try {
    callback()
    return ''
  } catch (error) {
    return error instanceof Error ? error.message : String(error)
  }
}
