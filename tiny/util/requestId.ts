export function generate(): string {
  const result = new Uint8Array(256)
  window.crypto.getRandomValues(result)
  return Array.from(result)
    .map((byte) => byte.toString(16).padStart(2, '0'))
    .join('')
}
