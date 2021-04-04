import * as payload from '@tiny/api/payload.ts'

export const checkMemberCreateRequest = payload.checkObject(
  {
    requestId: payload.checkString({ min: 64, max: 64 }),
    email: payload.checkString({ min: 5, max: 64 }),
    handle: payload.checkString({ min: 3, max: 64 }),
  }
)

export type MemberCreateRequest = payload.Payload<
  typeof checkMemberCreateRequest
>

export const checkMemberCreateResponse = payload.checkObject(
  {
    id: payload.checkString({ min: 64, max: 64 }),
  }
)

export type MemberCreateResponse = payload.Payload<
  typeof checkMemberCreateResponse
>

export const checkMemberCreateConflictResponse = payload.checkConflictResponse(
  'email',
  'handle'
)

export type MemberCreateConflictResponse = payload.Payload<
  typeof checkMemberCreateConflictResponse
>

export class MemberCreateConflictError extends payload.ConflictError<MemberCreateConflictResponse> {}
