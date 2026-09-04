// JavaScript Map uses SameValueZero for keys: primitives compare strictly,
// NaN compares equal to itself, and objects/arrays compare by identity.
export function same_value_zero(left, right) {
  return left === right || (left !== left && right !== right)
}

export function nan() {
  return Number.NaN
}
