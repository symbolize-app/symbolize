import * as appEndpointMember from '@fe/core/endpoint/member.ts'
import type * as appDbQuery from '@fe/db/query/index.ts'
import * as appDbQueryMember from '@fe/db/query/member.ts'
import * as route from '@tiny/api/route.ts'
import * as crypto from '@tiny/core/crypto.node.ts'
import type * as errorModule from '@tiny/core/error.ts'
import * as dbQuery from '@tiny/db/query.ts'

export const create = route.define(
  appEndpointMember.create,
  async (
    ctx: errorModule.Context & appDbQuery.WriteContext,
    request
  ) => {
    const id = crypto.hash(
      Buffer.from(request.json.requestId, 'hex')
    )
    const { email, handle } = request.json
    await dbQuery.retryDbQuery(
      ctx,
      'member create',
      appDbQueryMember.create,
      id,
      email,
      handle
    )
    return {
      status: 200,
      json: {
        id: id.toString('hex'),
      },
    }
  }
)

export const routes = [create]
