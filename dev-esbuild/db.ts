import beginTransaction from '@/query/begin_transaction.sql'
import commitTransaction from '@/query/commit_transaction.sql'
import insertPath from '@/query/insert_path.sql'
import insertVersion from '@/query/insert_version.sql'
import pragmaForeignKey from '@/query/pragma_foreign_key.sql'
import pragmaWalAutocheckpoint from '@/query/pragma_wal_autocheckpoint.sql'
import pragmaWalCheckpoint from '@/query/pragma_wal_checkpoint.sql'
import rollbackTransaction from '@/query/rollback_transaction.sql'
import updateContentCompressed from '@/query/update_content_compressed.sql'
import upsertContent from '@/query/upsert_content.sql'
import sqlite from 'better-sqlite3'
import * as nodeChildProcess from 'node:child_process'
import * as nodeUtil from 'node:util'

export type Database = Readonly<sqlite.Database>

export type Statement = Readonly<sqlite.Statement>

export interface Context {
  readonly db: Db
}

class Db {
  constructor(
    readonly query: {
      readonly beginTransaction: Statement
      readonly commitTransaction: Statement
      readonly insertPath: Statement
      readonly insertVersion: Statement
      readonly pragmaWalCheckpoint: string
      readonly rollbackTransaction: Statement
      readonly updateContentCompressed: Statement
      readonly upsertContent: Statement
    },
  ) {
    this.query = query
  }

  withTransactionSync<T>(action: () => T): T {
    try {
      this.query.beginTransaction.run()
      const result = action()
      this.query.commitTransaction.run()
      return result
    } catch (exception) {
      this.query.rollbackTransaction.run()
      throw exception
    }
  }
}

export type { Db }

export async function db(): Promise<Db> {
  await runMigrations()
  const connection = open()
  configure(connection)
  const query = prepareStatements(connection)
  return new Db(query)
}

function configure(connection: Database): void {
  connection.defaultSafeIntegers()
  connection.pragma(parsePragma(pragmaForeignKey))
  connection.pragma(parsePragma(pragmaWalAutocheckpoint))
}

function open(): Database {
  return sqlite('svc-gateway-host-store/build/manifest.sqlite3', {
    fileMustExist: true,
    readonly: false,
  })
}

function parsePragma(pragma: string): string {
  return pragma.replace(/^pragma /, '')
}

function prepareStatements(connection: Database): Db['query'] {
  return {
    beginTransaction: connection.prepare(beginTransaction),
    commitTransaction: connection.prepare(commitTransaction),
    insertPath: connection.prepare(insertPath),
    insertVersion: connection.prepare(insertVersion),
    pragmaWalCheckpoint: parsePragma(pragmaWalCheckpoint),
    rollbackTransaction: connection.prepare(rollbackTransaction),
    updateContentCompressed: connection.prepare(updateContentCompressed),
    upsertContent: connection.prepare(upsertContent),
  }
}

async function runMigrations(): Promise<void> {
  await nodeUtil.promisify(nodeChildProcess.exec)(
    'dbmate --no-dump-schema up',
  )
}
