import {createRequire} from 'node:module'
import {fileURLToPath} from 'node:url'

const require = createRequire(import.meta.url)
const BetterSQLite3 = require('./lib/index.js')
const nativeBinding = fileURLToPath(
  new URL('../../build/vendor/better-sqlite3/better_sqlite3.node', import.meta.url),
)

export default function DirectDatabase(filename, options) {
  return new BetterSQLite3(filename, {
    ...options,
    nativeBinding,
  })
}

DirectDatabase.prototype = BetterSQLite3.prototype
DirectDatabase.SqliteError = BetterSQLite3.SqliteError
