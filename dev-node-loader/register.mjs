import { register } from 'node:module'
import { fileURLToPath } from 'node:url'
import { dirname, resolve as pathResolve } from 'node:path'

const currentDir = dirname(fileURLToPath(import.meta.url))
const repoRoot = process.env.DEVENV_ROOT ?? pathResolve(currentDir, '..')

process.env.ESBUILD_BINARY_PATH ??= pathResolve(
  repoRoot,
  'build/vendor/esbuild/esbuild',
)
process.env.BETTER_SQLITE3_BINDING ??= pathResolve(
  repoRoot,
  'build/vendor/better-sqlite3/better_sqlite3.node',
)

register('./hooks.mjs', import.meta.url)
