import { Some } from '../gleam_stdlib/gleam/option.mjs'

export function new_ref(value) {
  return { value }
}

export function new_weak(value) {
  return new WeakRef(value)
}

export function deref_weak(reference) {
  const value = reference.deref()
  return value === undefined ? undefined : new Some(value)
}

export function read_ref(reference) {
  return reference.value
}

export function write_ref(reference, value) {
  reference.value = value
}

// Gleam's equality is structural on JavaScript values. The dataflow source
// commits only when JavaScript's strict identity comparison says the value
// changed, so that runtime semantic boundary stays explicit here.
export function same_value(first, second) {
  return first === second
}
