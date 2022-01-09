import * as appRoute from '@fe/api/route/index.ts'
import * as appEndpointTopic from '@fe/core/endpoint/topic.ts'
import type * as appDbQuery from '@fe/db/query/index.ts'
import * as appDbQueryTopic from '@fe/db/query/topic.ts'
import * as route from '@tiny/api/route.ts'
import * as crypto from '@tiny/core/crypto.node.ts'
import type * as errorModule from '@tiny/core/error.ts'
import * as dbQuery from '@tiny/db/query.ts'

export const create = route.defineEndpoint<
  errorModule.Context & appDbQuery.WriteContext
>(appEndpointTopic.create, async (ctx, request) => {
  const requestData = await appRoute.checkRequestJson(
    appEndpointTopic.create,
    request
  )
  const id = crypto.hash(
    Buffer.from(requestData.requestId, 'hex')
  )
  const memberId = Buffer.from(requestData.memberId, 'hex')
  const { title, slug, content } = requestData
  await appRoute.checkConflictQuery(
    appEndpointTopic.create,
    async () => {
      await dbQuery.retryDbQuery(
        ctx,
        'topic create',
        appDbQueryTopic.create,
        id,
        memberId,
        title,
        slug,
        content
      )
    }
  )
  return appRoute.checkOkResponse(appEndpointTopic.create, {
    id: id.toString('hex'),
  })
})

export const list = route.defineEndpoint<
  errorModule.Context & appDbQuery.ReadContext
>(appEndpointTopic.list, async (ctx, request) => {
  appRoute.checkRequestParams(
    appEndpointTopic.list,
    request
  )
  const results = await dbQuery.retryDbQuery(
    ctx,
    'topic list',
    appDbQueryTopic.list
  )
  return appRoute.checkOkResponse(appEndpointTopic.list, {
    results: results.map((row) => ({
      id: row.id.toString('hex'),
      updatedAt: row.updated_at.getTime(),
      title: row.title,
      slug: row.slug,
      content: row.content,
    })),
  })
})

export const update = route.defineEndpoint<
  errorModule.Context & appDbQuery.WriteContext
>(appEndpointTopic.update, async (ctx, request) => {
  const requestData = await appRoute.checkRequestJson(
    appEndpointTopic.update,
    request
  )
  const id = Buffer.from(requestData.id, 'hex')
  const updatedOld = new Date(requestData.updatedOld)
  const { title, slug, content } = requestData
  const result = await appRoute.checkConflictQuery(
    appEndpointTopic.create,
    async () => {
      return await dbQuery.retryDbQuery(
        ctx,
        'topic update',
        appDbQueryTopic.update,
        id,
        updatedOld,
        title,
        slug,
        content
      )
    }
  )
  if (result) {
    return appRoute.checkOkResponse(
      appEndpointTopic.update,
      {
        updated: result.updated.getTime(),
      }
    )
  } else {
    throw new route.ResponseError({
      status: 404,
    })
  }
})

export const routes = [create, list, update]
