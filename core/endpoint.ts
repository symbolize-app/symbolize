import * as appPayload from '@fe/core/payload.ts'
import * as endpoint from '@tiny/core/endpoint.ts'
import * as payload from '@tiny/core/payload.ts'

export const memberCreate = endpoint.defineConflictPostEndpoint(
  '/api/member/create',
  payload.checkObject({
    requestId: appPayload.checkId,
    email: appPayload.checkEmail,
    handle: appPayload.checkHandle,
  }),
  payload.checkObject({
    id: appPayload.checkId,
  }),
  payload.checkConflictResponse('email', 'handle')
)

export const topicCreate = endpoint.defineConflictPostEndpoint(
  '/api/topic/create',
  payload.checkObject({
    requestId: appPayload.checkId,
    memberId: appPayload.checkId,
    title: appPayload.checkTitle,
    slug: appPayload.checkSlug,
    content: appPayload.checkContent,
  }),
  payload.checkObject({
    id: appPayload.checkId,
  }),
  payload.checkConflictResponse('slug')
)

export const topicList = endpoint.defineGetEndpoint(
  '/api/topic/list',
  payload.checkObject({}),
  payload.checkObject({
    id: appPayload.checkId,
    title: appPayload.checkTitle,
    slug: appPayload.checkSlug,
    content: appPayload.checkContent,
  })
)

export const topicUpdate = endpoint.defineConflictPostEndpoint(
  '/api/topic/update',
  payload.checkObject({
    id: appPayload.checkId,
    updatedOld: payload.checkTimestamp,
    title: appPayload.checkTitle,
    slug: appPayload.checkSlug,
    content: appPayload.checkContent,
  }),
  payload.checkObject({
    updated: payload.checkTimestamp,
  }),
  payload.checkConflictResponse('slug', 'updatedOld')
)
