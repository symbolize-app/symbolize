import { toBitArray } from './gleam.mjs'
import { createHash } from 'node:crypto'
import {
  brotliCompressSync,
  brotliDecompressSync,
  constants,
} from 'node:zlib'

export function sha256(original) {
  return toBitArray([
    createHash('sha256').update(original.rawBuffer).digest(),
  ])
}

export function brotli_compress(original) {
  return toBitArray([
    brotliCompressSync(original.rawBuffer, {
      params: {
        [constants.BROTLI_PARAM_MODE]: constants.BROTLI_MODE_TEXT,
        [constants.BROTLI_PARAM_QUALITY]: constants.BROTLI_MAX_QUALITY,
      },
    }),
  ])
}

export function brotli_decompress(compressed) {
  return toBitArray([brotliDecompressSync(compressed.rawBuffer)])
}
