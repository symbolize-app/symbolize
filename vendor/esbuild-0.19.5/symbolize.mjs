import {fileURLToPath} from 'node:url'

const binaryPath = fileURLToPath(
  new URL('../../build/vendor/esbuild/esbuild', import.meta.url),
)

process.env.ESBUILD_BINARY_PATH ??= binaryPath

const {build} = await import(
  new URL('../../build/vendor/esbuild/main.js', import.meta.url),
)

export {build}
