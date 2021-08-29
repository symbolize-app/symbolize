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

export type Path = undefined | (() => (string | number)[])

export class PayloadError extends Error {
  constructor(message: string, path: Path) {
    super(
      `${message} at (root)${(path ? path() : [])
        .map((part) =>
          /^[A-Za-z_]\S*$/.exec(part.toString())
            ? `.${part}`
            : `[${JSON.stringify(part)}]`
        )
        .join('')}`
    )
  }
}

export function checkObject<
  Value extends {
    [Key in string]: typeFest.JsonValue
  } = Record<string, never>
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

export function checkArray<
  Value extends typeFest.JsonValue = never[]
>(config: Validator<Value>): Validator<Value[]> {
  return (input, path) => {
    if (
      input === null ||
      typeof input !== 'object' ||
      !Array.isArray(input)
    ) {
      throw new PayloadError(
        `Invalid array (wrong type ${getTypeName(input)})`,
        path
      )
    } else {
      const result = [] as Value[]
      for (let i = 0; i < input.length; i++) {
        const item = input[i]
        result[i] = config(item, buildPath(i, path))
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

export function checkStringMatch(config: {
  min: number
  max: number
  match: RegExp
}): Validator<string> {
  const base = checkString(config)
  return (baseInput, path) => {
    const input = base(baseInput, path)
    if (!config.match.exec(input)) {
      throw new PayloadError(
        `Invalid string match (for ${JSON.stringify(
          config.match.toString()
        )})`,
        path
      )
    } else {
      return input
    }
  }
}

export function checkNumber(config: {
  min: number
  max: number
}): Validator<number> {
  return (input, path) => {
    if (typeof input !== 'number') {
      throw new PayloadError(
        `Invalid number (wrong type ${getTypeName(input)})`,
        path
      )
    } else if (input < config.min) {
      throw new PayloadError(
        `Invalid number (too small, min ${config.min})`,
        path
      )
    } else if (input > config.max) {
      throw new PayloadError(
        `Invalid number (too large, max ${config.max})`,
        path
      )
    } else {
      return input
    }
  }
}

export function checkInteger(config: {
  min: number
  max: number
}): Validator<number> {
  const base = checkNumber(config)
  return (baseInput, path) => {
    const input = base(baseInput, path)
    if ((input | 0) !== input) {
      throw new PayloadError(
        'Invalid integer (includes fractional component)',
        path
      )
    } else {
      return input
    }
  }
}

const checkTimestampBase = checkNumber({
  min: Date.UTC(1000, 1, 1),
  max: Date.UTC(10000, 1, 1),
})
export const checkTimestamp: Validator<number> = (
  baseInput,
  path
) => {
  const input = checkTimestampBase(baseInput, path)
  if (input % 1000 !== 0) {
    throw new PayloadError(
      'Invalid timestamp (includes fractional seconds)',
      path
    )
  } else {
    return input
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
  part: string | number,
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
