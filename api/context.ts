import type * as db from '@fe/db/index.ts'
import type * as errorModule from '@tiny/util/error.ts'

export type Context = errorModule.RetryContext &
  DatabaseReadContext &
  DatabaseWriteContext

export type DatabaseReadContext = {
  databaseApiRead: db.DatabaseApiRead
}

export type DatabaseWriteContext = {
  databaseApiWrite: db.DatabaseApiWrite
}
