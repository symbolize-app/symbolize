import type * as db from '@fe/db/index.ts'
import topicCreate from '@fe/db/query/topicCreate.sql'
import topicList from '@fe/db/query/topicList.sql'
import topicUpdate from '@fe/db/query/topicUpdate.sql'
import * as query from '@tiny/db/query.ts'

export const create = query.defineVoid<
  db.DatabaseApiWrite,
  [
    id: Buffer,
    memberId: Buffer,
    title: string,
    slug: string,
    content: string
  ]
>(topicCreate)

export const list = query.defineMulti<
  db.DatabaseApiRead,
  [],
  {
    id: Buffer
    title: string
    slug: string
    content: string
  }
>(topicList)

export const update = query.defineOptional<
  db.DatabaseApiWrite,
  [
    id: Buffer,
    updatedOld: Date,
    title: string,
    slug: string,
    content: string
  ],
  { updated: Date }
>(topicUpdate)
