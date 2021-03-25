import type * as query from '@tiny/db/index.ts'
import type * as typeFest from 'type-fest'

declare const databaseApiReadSymbol: unique symbol

export type DatabaseApiRead = typeFest.Opaque<
  query.Database,
  typeof databaseApiReadSymbol
>

declare const databaseApiWriteSymbol: unique symbol

export type DatabaseApiWrite = typeFest.Opaque<
  query.Database,
  typeof databaseApiWriteSymbol
>
