import * as appEndpointTopic from '@fe/core/endpoint/topic.ts'
import type * as appDbQuery from '@fe/db/query/index.ts'
import * as appDbQueryTopic from '@fe/db/query/topic.ts'
import * as route from '@tiny/api/route.ts'
import * as crypto from '@tiny/core/crypto.node.ts'
import type * as errorModule from '@tiny/core/error.ts'
import * as dbQuery from '@tiny/db/query.ts'

export const create = route.define(
  appEndpointTopic.create,
  async (
    ctx: errorModule.Context & appDbQuery.WriteContext,
    request
  ) => {
    const id = crypto.hash(
      Buffer.from(request.json.requestId, 'hex')
    )
    const memberId = Buffer.from(
      request.json.memberId,
      'hex'
    )
    const { title, slug, content } = request.json
    await dbQuery.retryQuery(
      ctx,
      'topic create',
      appDbQueryTopic.create,
      id,
      memberId,
      title,
      slug,
      content
    )
    return {
      status: 200,
      headers: {
        'content-type': 'application/json',
      },
      json: {
        id: id.toString('hex'),
      },
    }
  }
)

export const list = route.define(
  appEndpointTopic.list,
  async (
    ctx: errorModule.Context & appDbQuery.ReadContext,
    _request
  ) => {
    const results = await dbQuery.retryQuery(
      ctx,
      'topic list',
      appDbQueryTopic.list
    )
    return {
      status: 200,
      json: {
        results: results.map((row) => ({
          id: row.id.toString('hex'),
          updatedAt: row.updated_at.getTime(),
          title: row.title,
          slug: row.slug,
          content: row.content,
        })),
      },
    }
  }
)

export const update = route.define(
  appEndpointTopic.update,
  async (
    ctx: errorModule.Context & appDbQuery.WriteContext,
    request
  ) => {
    const id = Buffer.from(request.json.id, 'hex')
    const updatedOld = new Date(request.json.updatedOld)
    const { title, slug, content } = request.json
    const result = await dbQuery.retryQuery(
      ctx,
      'topic update',
      appDbQueryTopic.update,
      id,
      updatedOld,
      title,
      slug,
      content
    )
    if (result) {
      return {
        status: 200,
        json: {
          updated: result.updated.getTime(),
        },
      }
    } else {
      throw new route.ResponseError({
        status: 404,
      })
    }
  }
)

export const routes = [create, list, update]
