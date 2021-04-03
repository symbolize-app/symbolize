export function uint8ArrayToHex(input: Uint8Array): string {
  return Array.from(input)
    .map((byte) => byte.toString(16).padStart(2, '0'))
    .join('')
}

export function uint8ArrayFromHex(
  input: string
): Uint8Array {
  return new Uint8Array(
    input
      .split(/(..)/)
      .filter(Boolean)
      .map((byte) => parseInt(byte, 16))
  )
}
