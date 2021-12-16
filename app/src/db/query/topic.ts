import dbQueryTopicCreate from '@db/query/topic_create.sql'
import dbQueryTopicList from '@db/query/topic_list.sql'
import dbQueryTopicUpdate from '@db/query/topic_update.sql'
import type * as appQuery from '@fe/db/query/index.ts'
import * as query from '@tiny/db/query.ts'

export const create = query.defineVoid<
  appQuery.Write,
  [
    id: Buffer,
    memberId: Buffer,
    title: string,
    slug: string,
    content: string
  ]
>(dbQueryTopicCreate)

export const list = query.defineMulti<
  appQuery.Read,
  [],
  {
    id: Buffer
    updated_at: Date
    title: string
    slug: string
    content: string
  }
>(dbQueryTopicList)

export const update = query.defineOptional<
  appQuery.Write,
  [
    id: Buffer,
    updatedOld: Date,
    title: string,
    slug: string,
    content: string
  ],
  { updated: Date }
>(dbQueryTopicUpdate)
