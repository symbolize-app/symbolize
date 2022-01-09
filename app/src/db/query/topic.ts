import dbQueryTopicCreate from '@db/query/topic_create.sql'
import dbQueryTopicList from '@db/query/topic_list.sql'
import dbQueryTopicUpdate from '@db/query/topic_update.sql'
import * as appDbQuery from '@fe/db/query/index.ts'
import * as dbQuery from '@tiny/db/query.ts'

export const create = dbQuery.defineVoid(
  appDbQuery.write,
  dbQueryTopicCreate,
  {
    values:
      dbQuery.values<
        [
          id: Buffer,
          memberId: Buffer,
          title: string,
          slug: string,
          content: string
        ]
      >(),
    conflictMap: {
      ['primary']: 'slug',
    },
  }
)

export const list = dbQuery.defineMulti(
  appDbQuery.read,
  dbQueryTopicList,
  {
    values: dbQuery.values<[]>(),
    /* eslint-disable @typescript-eslint/naming-convention */
    row: dbQuery.row<{
      id: Buffer
      updated_at: Date
      title: string
      slug: string
      content: string
    }>(),
    /* eslint-enable @typescript-eslint/naming-convention */
  }
)

export const update = dbQuery.defineOptional(
  appDbQuery.write,
  dbQueryTopicUpdate,
  {
    values:
      dbQuery.values<
        [
          id: Buffer,
          updatedOld: Date,
          title: string,
          slug: string,
          content: string
        ]
      >(),
    row: dbQuery.row<{ updated: Date }>(),
    conflictMap: {
      ['primary']: 'slug',
    },
  }
)
