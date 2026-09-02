export function new_identity() {
  return Symbol()
}

export function same_identity(first, second) {
  return first === second
}
