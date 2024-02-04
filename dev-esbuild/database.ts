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

export type Database = sqlite.Database

export type Statement = sqlite.Statement

export interface Context {
  db: {
    connection: Database
    query: {
      beginTransaction: Statement
      commitTransaction: Statement
      insertPath: Statement
      insertVersion: Statement
      pragmaWalCheckpoint: string
      rollbackTransaction: Statement
      updateContentCompressed: Statement
      upsertContent: Statement
    }
  }
}

export async function initContext(): Promise<Context> {
  await runMigrations()
  const connection = open()
  configure(connection)
  const query = prepareStatements(connection)
  return { db: { connection, query } }
}

async function runMigrations(): Promise<void> {
  await nodeUtil.promisify(nodeChildProcess.exec)(
    'dbmate --no-dump-schema up'
  )
}

function open(): Database {
  return sqlite('svc-gateway-host-store/build/manifest.sqlite3', {
    fileMustExist: true,
    readonly: false,
  })
}

function configure(connection: Database): void {
  connection.defaultSafeIntegers()
  connection.pragma(parsePragma(pragmaForeignKey))
  connection.pragma(parsePragma(pragmaWalAutocheckpoint))
}

function prepareStatements(connection: Database): Context['db']['query'] {
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

function parsePragma(pragma: string): string {
  return pragma.replace(/^pragma /, '')
}

export function withTransactionSync<T>(ctx: Context, action: () => T): T {
  try {
    ctx.db.query.beginTransaction.run()
    const result = action()
    ctx.db.query.commitTransaction.run()
    return result
  } catch (exception) {
    ctx.db.query.rollbackTransaction.run()
    throw exception
  }
}
