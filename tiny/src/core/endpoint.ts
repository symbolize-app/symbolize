import type * as payload from '@tiny/core/payload.ts'

export type RequestParamsEndpoint = {
  requestParams: payload.StringValidator
}

export type RequestStreamEndpoint = {
  requestStream: true
}

export type RequestJsonEndpoint = {
  requestJson: payload.Validator
}

export type OkResponseStreamEndpoint = {
  okResponseStream: true
}

export type OkResponseJsonEndpoint = {
  okResponseJson: payload.Validator
}

export type ConflictResponseJsonEndpoint = {
  conflictResponseJson: payload.ConflictValidator
}

type PartialChecks = Partial<
  RequestParamsEndpoint &
    RequestJsonEndpoint &
    RequestStreamEndpoint &
    OkResponseStreamEndpoint &
    OkResponseJsonEndpoint &
    ConflictResponseJsonEndpoint
>

export type BaseEndpoint<
  Method extends string = string,
  Checks extends PartialChecks = PartialChecks
> = {
  method: Method
  path: string
} & Checks

function defineBaseEndpoint<
  Method extends string,
  Checks extends PartialChecks
>(
  method: Method,
  path: string,
  checks: Checks
): BaseEndpoint<Method, Checks> {
  return {
    method,
    path,
    ...checks,
  }
}

export type GetEndpoint<
  Checks extends PartialChecks = PartialChecks
> = BaseEndpoint<'GET', Checks>

export function defineGetEndpoint<
  Checks extends PartialChecks
>(path: string, checks: Checks): GetEndpoint<Checks> {
  return defineBaseEndpoint('GET', path, checks)
}

export type PostEndpoint<
  Checks extends PartialChecks = PartialChecks
> = BaseEndpoint<'POST', Checks>

export function definePostEndpoint<
  Checks extends PartialChecks
>(path: string, checks: Checks): PostEndpoint<Checks> {
  return defineBaseEndpoint('POST', path, checks)
}
