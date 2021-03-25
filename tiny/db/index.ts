import type pg from 'pg'

export type Database = {
  pool: pg.Pool
}
