import type * as typeFest from 'type-fest'

export type Payload<CustomValidator extends Validator> =
  ReturnType<CustomValidator['check']>

export type Validator<
  Value extends typeFest.JsonValue = typeFest.JsonValue
> = {
  check: (input: typeFest.JsonValue, path?: Path) => Value
}

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

export function nullOr<Value extends typeFest.JsonValue>(
  config: Validator<Value>
): Validator<Value | null> {
  return {
    check(input, path) {
      if (input === null) {
        return null
      } else {
        return config.check(input, path)
      }
    },
  }
}

export function object<
  Value extends {
    [Key in string]: typeFest.JsonValue
  } = Record<string, never>
>(config: {
  [Key in keyof Value]: Validator<Value[Key]>
}): Validator<Value> {
  return {
    check(input, path) {
      if (
        input === null ||
        typeof input !== 'object' ||
        Array.isArray(input)
      ) {
        throw new PayloadError(
          `Invalid object (wrong type ${getTypeName(
            input
          )})`,
          path
        )
      } else {
        const result = {} as Value
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
            result[key] = config[key].check(
              value,
              buildPath(key, path)
            )
          }
        }
        return result
      }
    },
  }
}

export function array<
  Value extends typeFest.JsonValue = never[]
>(config: Validator<Value>): Validator<Value[]> {
  return {
    check(input, path) {
      if (
        input === null ||
        typeof input !== 'object' ||
        !Array.isArray(input)
      ) {
        throw new PayloadError(
          `Invalid array (wrong type ${getTypeName(
            input
          )})`,
          path
        )
      } else {
        const result = [] as Value[]
        for (let i = 0; i < input.length; i++) {
          const item = input[i]
          result[i] = config.check(item, buildPath(i, path))
        }
        return result
      }
    },
  }
}

export function string(config: {
  min: number
  max: number
}): Validator<string> {
  return {
    check(input, path) {
      if (typeof input !== 'string') {
        throw new PayloadError(
          `Invalid string (wrong type ${getTypeName(
            input
          )})`,
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
    },
  }
}

export function stringOption<Options extends string>(
  ...options: Options[]
): Validator<Options> {
  return {
    check(input, path) {
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
    },
  }
}

export function stringMatch(config: {
  min: number
  max: number
  match: RegExp
}): Validator<string> {
  const base = string(config)
  return {
    check(baseInput, path) {
      const input = base.check(baseInput, path)
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
    },
  }
}

export function stringEnum<Value extends string>(
  mapping: Record<string, Value>
): Validator<Value> {
  return {
    check(input, path) {
      if (typeof input !== 'string') {
        throw new PayloadError(
          `Invalid enum (wrong type ${getTypeName(input)})`,
          path
        )
      } else {
        const values = Object.values(mapping)
        for (const value of values) {
          if (input === value) {
            return value
          }
        }
        throw new PayloadError(
          `Invalid enum (not ${values
            .map((value) => JSON.stringify(value))
            .join(' | ')})`,
          path
        )
      }
    },
  }
}

export function number(config: {
  min: number
  max: number
}): Validator<number> {
  return {
    check(input, path) {
      if (typeof input !== 'number') {
        throw new PayloadError(
          `Invalid number (wrong type ${getTypeName(
            input
          )})`,
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
    },
  }
}

export function integer(config: {
  min: number
  max: number
}): Validator<number> {
  const base = number(config)
  return {
    check(baseInput, path) {
      const input = base.check(baseInput, path)
      if ((input | 0) !== input) {
        throw new PayloadError(
          'Invalid integer (includes fractional component)',
          path
        )
      } else {
        return input
      }
    },
  }
}

const timestampBase = number({
  min: Date.UTC(1000, 1, 1),
  max: Date.UTC(10000, 1, 1),
})
export const timestamp: Validator<number> = {
  check(baseInput, path) {
    const input = timestampBase.check(baseInput, path)
    if (input % 1000 !== 0) {
      throw new PayloadError(
        'Invalid timestamp (includes fractional seconds)',
        path
      )
    } else {
      return input
    }
  },
}

export class ConflictError<
  Field extends string
> extends Error {
  field: Field
  constructor(field: Field) {
    super(`Conflict on ${JSON.stringify(field)}`)
    this.field = field
  }
}

export type ConflictValidator<
  Field extends string = string
> = Validator<{ conflict: Field }> & {
  error: new (field: never) => ConflictError<Field>
}

export function conflict<Field extends string>(
  ...options: Field[]
): ConflictValidator<Field> {
  class CustomConflictError extends ConflictError<Field> {}
  return {
    ...object({
      conflict: stringOption(...options),
    }),
    error: CustomConflictError,
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
