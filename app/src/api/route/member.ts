import * as appEndpointMember from '@fe/core/endpoint/member.ts'
import type * as appDbQuery from '@fe/db/query/index.ts'
import * as appDbQueryMember from '@fe/db/query/member.ts'
import * as tinyRoute from '@tiny/api/route.ts'
import * as tinyCrypto from '@tiny/core/crypto.node.ts'
import type * as tinyError from '@tiny/core/error.ts'
import * as tinyDbQuery from '@tiny/db/query.ts'

export const create = tinyRoute.define(
  appEndpointMember.create,
  async (
    ctx: tinyError.Context & appDbQuery.WriteContext,
    request
  ) => {
    const id = tinyCrypto.hash(
      Buffer.from(request.json.requestId, 'hex')
    )
    const { email, handle } = request.json
    await tinyDbQuery.retryQuery(
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
