import * as appRoute from '@fe/api/route/index.ts'
import * as appEndpointTopic from '@fe/core/endpoint/topic.ts'
import type * as appQuery from '@fe/db/query/index.ts'
import * as appQueryTopic from '@fe/db/query/topic.ts'
import * as route from '@tiny/api/route.ts'
import * as crypto from '@tiny/core/crypto.node.ts'
import type * as errorModule from '@tiny/core/error.ts'

export const create = route.defineEndpoint<
  errorModule.Context & appQuery.WriteContext
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
  await appRoute.retryConflictQuery(
    ctx,
    'topic create',
    appEndpointTopic.create,
    {
      ['primary']: 'slug',
    },
    () =>
      ctx.databaseApiWrite.query(
        appQueryTopic.create,
        id,
        memberId,
        title,
        slug,
        content
      )
  )
  return appRoute.checkOkResponse(appEndpointTopic.create, {
    id: id.toString('hex'),
  })
})

export const list = route.defineEndpoint<
  errorModule.Context & appQuery.ReadContext
>(appEndpointTopic.list, async (ctx, request) => {
  appRoute.checkRequestParams(
    appEndpointTopic.list,
    request
  )
  const results = await appRoute.retryQuery(
    ctx,
    'topic list',
    () => ctx.databaseApiRead.query(appQueryTopic.list)
  )
  return appRoute.checkOkResponse(appEndpointTopic.list, {
    results: results.map((row) => ({
      id: row.id.toString('hex'),
      updated: row.updated.getTime(),
      title: row.title,
      slug: row.slug,
      content: row.content,
    })),
  })
})

export const update = route.defineEndpoint<
  errorModule.Context & appQuery.WriteContext
>(appEndpointTopic.update, async (ctx, request) => {
  const requestData = await appRoute.checkRequestJson(
    appEndpointTopic.update,
    request
  )
  const id = crypto.hash(Buffer.from(requestData.id, 'hex'))
  const updatedOld = new Date(requestData.updatedOld)
  const { title, slug, content } = requestData
  const result = await appRoute.retryConflictQuery(
    ctx,
    'topic update',
    appEndpointTopic.update,
    {
      ['primary']: 'slug',
    },
    () =>
      ctx.databaseApiWrite.query(
        appQueryTopic.update,
        id,
        updatedOld,
        title,
        slug,
        content
      )
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
