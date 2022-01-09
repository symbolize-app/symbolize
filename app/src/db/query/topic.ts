import dbQueryTopicCreate from '@db/query/topic_create.sql'
import dbQueryTopicList from '@db/query/topic_list.sql'
import dbQueryTopicUpdate from '@db/query/topic_update.sql'
import type * as appDbQuery from '@fe/db/query/index.ts'
import * as dbQuery from '@tiny/db/query.ts'

export const create = dbQuery.defineVoid<
  appDbQuery.Write,
  [
    id: Buffer,
    memberId: Buffer,
    title: string,
    slug: string,
    content: string
  ]
>(dbQueryTopicCreate, {
  conflictMap: {
    ['primary']: 'slug',
  },
})

export const list = dbQuery.defineMulti<
  appDbQuery.Read,
  [],
  /* eslint-disable @typescript-eslint/naming-convention */
  {
    id: Buffer
    updated_at: Date
    title: string
    slug: string
    content: string
  }
  /* eslint-enable @typescript-eslint/naming-convention */
>(dbQueryTopicList, {})

export const update = dbQuery.defineOptional<
  appDbQuery.Write,
  [
    id: Buffer,
    updatedOld: Date,
    title: string,
    slug: string,
    content: string
  ],
  { updated: Date }
>(dbQueryTopicUpdate, {
  conflictMap: {
    ['primary']: 'slug',
  },
})
