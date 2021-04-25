import * as payload from '@tiny/api/payload.ts'

// TODO Move to core
// TODO Create endpoints

const checkId = payload.checkString({ min: 64, max: 64 })

const checkEmail = payload.checkString({ min: 5, max: 64 })

const checkHandle = payload.checkString({ min: 3, max: 64 })

const checkTitle = payload.checkString({ min: 3, max: 128 })

const checkSlug = payload.checkString({ min: 3, max: 128 })

const checkContent = payload.checkString({
  min: 3,
  max: 4096,
})

export const checkMemberCreateRequest = payload.checkObject(
  {
    requestId: checkId,
    email: checkEmail,
    handle: checkHandle,
  }
)

export type MemberCreateRequest = payload.Payload<
  typeof checkMemberCreateRequest
>

export const checkMemberCreateOkResponse = payload.checkObject(
  {
    id: checkId,
  }
)

export type MemberCreateOkResponse = payload.Payload<
  typeof checkMemberCreateOkResponse
>

export const checkMemberCreateConflictResponse = payload.checkConflictResponse(
  'email',
  'handle'
)

export type MemberCreateConflictResponse = payload.Payload<
  typeof checkMemberCreateConflictResponse
>

export class MemberCreateConflictError extends payload.ConflictError<MemberCreateConflictResponse> {}

export const checkTopicCreateRequest = payload.checkObject({
  requestId: checkId,
  memberId: checkId,
  title: checkTitle,
  slug: checkSlug,
  content: checkContent,
})

export type TopicCreateRequest = payload.Payload<
  typeof checkTopicCreateRequest
>

export const checkTopicCreateOkResponse = payload.checkObject(
  {
    id: checkId,
  }
)

export type TopicCreateOkResponse = payload.Payload<
  typeof checkTopicCreateOkResponse
>

export const checkTopicCreateConflictResponse = payload.checkConflictResponse(
  'slug'
)

export type TopicCreateConflictResponse = payload.Payload<
  typeof checkTopicCreateConflictResponse
>

export class TopicCreateConflictError extends payload.ConflictError<TopicCreateConflictResponse> {}

export const checkTopicListRequest = payload.checkObject({})

export type TopicListRequest = payload.Payload<
  typeof checkTopicListRequest
>

export const checkTopicListOkResponse = payload.checkObject(
  {
    id: checkId,
    title: checkTitle,
    slug: checkSlug,
    continue: checkContent,
  }
)

export type TopicListOkResponse = payload.Payload<
  typeof checkTopicListOkResponse
>

export const checkTopicUpdateRequest = payload.checkObject({
  id: checkId,
  updatedOld: payload.checkTimestamp,
  title: checkTitle,
  slug: checkSlug,
  content: checkContent,
})

export type TopicUpdateRequest = payload.Payload<
  typeof checkTopicUpdateRequest
>

export const checkTopicUpdateOkResponse = payload.checkObject(
  {
    updated: payload.checkTimestamp,
  }
)

export type TopicUpdateOkResponse = payload.Payload<
  typeof checkTopicUpdateOkResponse
>

export const checkTopicUpdateConflictResponse = payload.checkConflictResponse(
  'slug',
  'updatedOld'
)

export type TopicUpdateConflictResponse = payload.Payload<
  typeof checkTopicUpdateConflictResponse
>

export class TopicUpdateConflictError extends payload.ConflictError<TopicUpdateConflictResponse> {}
