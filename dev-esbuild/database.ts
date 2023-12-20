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

export type Context = {
  db: Database
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

export async function initContext(): Promise<Context> {
  await runMigrations()
  const db = open()
  configure(db)
  const query = prepareStatements(db)
  return { db, query }
}

async function runMigrations(): Promise<void> {
  await nodeUtil.promisify(nodeChildProcess.exec)(
    'dbmate --no-dump-schema up'
  )
}

function open(): Database {
  return sqlite('svc-gateway-host-store/build/manifest.sqlite3', {
    readonly: false,
    fileMustExist: true,
  })
}

function configure(db: Database): void {
  db.defaultSafeIntegers()
  db.pragma(parsePragma(pragmaForeignKey))
  db.pragma(parsePragma(pragmaWalAutocheckpoint))
}

function prepareStatements(db: Database): Context['query'] {
  return {
    beginTransaction: db.prepare(beginTransaction),
    commitTransaction: db.prepare(commitTransaction),
    insertPath: db.prepare(insertPath),
    insertVersion: db.prepare(insertVersion),
    pragmaWalCheckpoint: parsePragma(pragmaWalCheckpoint),
    rollbackTransaction: db.prepare(rollbackTransaction),
    updateContentCompressed: db.prepare(updateContentCompressed),
    upsertContent: db.prepare(upsertContent),
  }
}

function parsePragma(pragma: string): string {
  return pragma.replace(/^pragma /, '')
}

export function withTransactionSync<T>(ctx: Context, action: () => T): T {
  try {
    ctx.query.beginTransaction.run()
    const result = action()
    ctx.query.commitTransaction.run()
    return result
  } catch (exception) {
    ctx.query.rollbackTransaction.run()
    throw exception
  }
}
