import dbQueryTopicCreate from '@db/query/topic_create.sql'
import dbQueryTopicList from '@db/query/topic_list.sql'
import dbQueryTopicUpdate from '@db/query/topic_update.sql'
import * as appDbQuery from '@fe/db/query/index.ts'
import * as tinyDbQuery from '@tiny/db/query.ts'

export const create = tinyDbQuery.defineVoid(
  appDbQuery.write,
  dbQueryTopicCreate,
  {
    params:
      tinyDbQuery.params<
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

export const list = tinyDbQuery.defineMulti(
  appDbQuery.read,
  dbQueryTopicList,
  {
    params: tinyDbQuery.params<[]>(),
    /* eslint-disable @typescript-eslint/naming-convention */
    row: tinyDbQuery.row<{
      id: Buffer
      updated_at: Date
      title: string
      slug: string
      content: string
    }>(),
    /* eslint-enable @typescript-eslint/naming-convention */
  }
)

export const update = tinyDbQuery.defineOptional(
  appDbQuery.write,
  dbQueryTopicUpdate,
  {
    params:
      tinyDbQuery.params<
        [
          id: Buffer,
          updatedOld: Date,
          title: string,
          slug: string,
          content: string
        ]
      >(),
    row: tinyDbQuery.row<{ updated: Date }>(),
    conflictMap: {
      ['primary']: 'slug',
    },
  }
)
