import { toBitArray } from './gleam.mjs'

export function new_system_random() {
  return {}
}

export function crypto_bits(_random, byteCount) {
  const result = new Uint8Array(byteCount)
  globalThis.crypto.getRandomValues(result)
  return toBitArray([result])
}

export function math_random(_random) {
  return globalThis.Math.random()
}
