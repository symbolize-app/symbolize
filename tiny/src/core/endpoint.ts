export type BaseEndpoint<
  Method extends string = string,
  Checks = unknown
> = {
  method: Method
  path: string
} & Checks

function defineBaseEndpoint<Method extends string, Checks>(
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

export type GetEndpoint<Checks = unknown> = BaseEndpoint<
  'GET',
  Checks
>

export function defineGetEndpoint<Checks>(
  path: string,
  checks: Checks
): GetEndpoint<Checks> {
  return defineBaseEndpoint('GET', path, checks)
}

export type PostEndpoint<Checks = unknown> = BaseEndpoint<
  'POST',
  Checks
>

export function definePostEndpoint<Checks>(
  path: string,
  checks: Checks
): PostEndpoint<Checks> {
  return defineBaseEndpoint('POST', path, checks)
}
