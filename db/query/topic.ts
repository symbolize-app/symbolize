import type * as appQuery from '@fe/db/query/index.ts'
import appQueryTopicCreate from '@fe/db/query/topicCreate.sql'
import appQueryTopicList from '@fe/db/query/topicList.sql'
import appQueryTopicUpdate from '@fe/db/query/topicUpdate.sql'
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
>(appQueryTopicCreate)

export const list = query.defineMulti<
  appQuery.Read,
  [],
  {
    id: Buffer
    updated: Date
    title: string
    slug: string
    content: string
  }
>(appQueryTopicList)

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
>(appQueryTopicUpdate)
