import sodiumNative from 'sodium-native'

export function hash(input: Buffer): Buffer {
  const output = Buffer.alloc(
    sodiumNative.crypto_generichash_BYTES
  )
  sodiumNative.crypto_generichash(output, input)
  return output
}
