import type * as typeFest from 'type-fest'

export type Payload<
  CustomValidator extends Validator<typeFest.JsonValue>
> = CustomValidator extends Validator<infer Value>
  ? Value
  : never

export type Validator<Value extends typeFest.JsonValue> = (
  input: typeFest.JsonValue,
  path?: Path
) => Value

export type Path = undefined | (() => string[])

export class PayloadError extends Error {
  constructor(message: string, path: Path) {
    super(
      `${message} at (root)${(path ? path() : [])
        .map((part) =>
          /^[A-Za-z_]\S*$/.exec(part)
            ? `.${part}`
            : `[${JSON.stringify(part)}]`
        )
        .join('')}`
    )
  }
}

export function checkObject<
  Value extends { [Key in string]: typeFest.JsonValue }
>(
  config: { [Key in keyof Value]: Validator<Value[Key]> }
): Validator<Value> {
  return (input, path) => {
    if (
      input === null ||
      typeof input !== 'object' ||
      Array.isArray(input)
    ) {
      throw new PayloadError(
        `Invalid object (wrong type ${getTypeName(input)})`,
        path
      )
    } else {
      const result = {} as Value
      for (const key in input) {
        if (!(key in config)) {
          throw new PayloadError(
            `Invalid object (extra key ${JSON.stringify(
              key
            )})`,
            path
          )
        }
      }
      for (const key in config) {
        const value = input[key]
        if (value === undefined) {
          throw new PayloadError(
            `Invalid object (missing key ${JSON.stringify(
              key
            )})`,
            path
          )
        } else {
          result[key] = config[key](
            value,
            buildPath(key, path)
          )
        }
      }
      return result
    }
  }
}

export function checkString(config: {
  min: number
  max: number
}): Validator<string> {
  return (input, path) => {
    if (typeof input !== 'string') {
      throw new PayloadError(
        `Invalid string (wrong type ${getTypeName(input)})`,
        path
      )
    } else if (input.length < config.min) {
      throw new PayloadError(
        `Invalid string (too short, min ${config.min})`,
        path
      )
    } else if (input.length > config.max) {
      throw new PayloadError(
        `Invalid string (too long, max ${config.max})`,
        path
      )
    } else {
      return input
    }
  }
}

export function checkStringOption<Options extends string>(
  ...options: Options[]
): Validator<Options> {
  return (input, path) => {
    if (typeof input !== 'string') {
      throw new PayloadError(
        `Invalid string option (wrong type ${getTypeName(
          input
        )})`,
        path
      )
    } else {
      for (const option of options) {
        if (input === option) {
          return option
        }
      }
      throw new PayloadError(
        `Invalid string option (not ${options
          .map((option) => JSON.stringify(option))
          .join(' | ')})`,
        path
      )
    }
  }
}

export function checkConflictResponse<Field extends string>(
  ...options: Field[]
): Validator<{ conflict: Field }> {
  return checkObject({
    conflict: checkStringOption(...options),
  })
}

export class ConflictError<
  ConflictResponse extends { conflict: string }
> extends Error {
  field: ConflictResponse['conflict']
  constructor(field: ConflictResponse['conflict']) {
    super(`Conflict on ${JSON.stringify(field)}`)
    this.field = field
  }
}

export function buildPath(
  part: string,
  parentPath: Path
): Path {
  return () => [...(parentPath ? parentPath() : []), part]
}

export function getTypeName(
  input: typeFest.JsonValue
): string {
  const typeofName = typeof input
  if (typeofName !== 'object') {
    return typeofName
  } else if (input === null) {
    return 'null'
  } else if (Array.isArray(input)) {
    return 'array'
  } else {
    return 'object'
  }
}
