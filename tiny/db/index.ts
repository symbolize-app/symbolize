import type pg from 'pg'

export type Database = {
  pool: Pick<pg.Pool, 'query'>
}
