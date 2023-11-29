export type JsonValue =
  | JsonObject
  | JsonArray
  | boolean
  | number
  | string
  | null

export type JsonObject = { [Key in string]: JsonValue }

export type JsonArray = JsonValue[]

export type JsonPayload<
  CustomTransformer extends JsonPayloadTransformer<unknown>,
> = CustomTransformer extends JsonPayloadTransformer<
  infer T
>
  ? T
  : never

export type JsonPayloadTransformer<Value> = {
  fromJson: (input: JsonValue, path?: Path) => Value
  toJson: (output: Value, path?: Path) => JsonValue
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

export function nullOr<Value>(
  config: JsonPayloadTransformer<Value>
): JsonPayloadTransformer<Value | null> {
  return {
    fromJson(input, path) {
      if (input === null) {
        return null
      } else {
        return config.fromJson(input, path)
      }
    },
    toJson(output, path) {
      if (output === null) {
        return null
      } else {
        return config.toJson(output, path)
      }
    },
  }
}

export function object<
  Value extends Record<never, never>,
>(config: {
  [Key in keyof Value]: JsonPayloadTransformer<Value[Key]>
}): JsonPayloadTransformer<Value> {
  return {
    fromJson(input, path) {
      checkObject(input, path)
      const result = {} as Value
      for (const key in config) {
        const value = input[key]
        checkValue(value, key, path)
        result[key] = config[key].fromJson(
          value,
          buildPath(key, path)
        )
      }
      return result
    },
    toJson(output, path) {
      checkObject(output, path)
      const result = {} as JsonObject
      for (const key in config) {
        const value = output[key]
        checkValue(value, key, path)
        result[key] = config[key].toJson(
          value,
          buildPath(key, path)
        )
      }
      return result
    },
  }
}

function checkObject(
  value: unknown,
  path: Path
): asserts value is Record<
  string | symbol | number,
  unknown
> {
  if (
    value === null ||
    typeof value !== 'object' ||
    Array.isArray(value)
  ) {
    throw new PayloadError(
      `Invalid object (wrong type ${getTypeName(value)})`,
      path
    )
  }
}

function checkValue(
  value: unknown,
  key: string,
  path: Path
): void {
  if (value === undefined) {
    throw new PayloadError(
      `Invalid object (missing key ${JSON.stringify(key)})`,
      path
    )
  }
}

export function array<Value>(
  config: JsonPayloadTransformer<Value>
): JsonPayloadTransformer<Value[]> {
  return {
    fromJson(input, path) {
      checkArray(input, path)
      const result = [] as Value[]
      for (let i = 0; i < input.length; i++) {
        const item = input[i]
        result[i] = config.fromJson(
          item,
          buildPath(i, path)
        )
      }
      return result
    },
    toJson(output, path) {
      checkArray(output, path)
      const result = [] as JsonArray
      for (let i = 0; i < output.length; i++) {
        const item = output[i]
        result[i] = config.toJson(item, buildPath(i, path))
      }
      return result
    },
  }
}

function checkArray(
  value: unknown,
  path: Path
): asserts value is unknown[] {
  if (
    value === null ||
    typeof value !== 'object' ||
    !Array.isArray(value)
  ) {
    throw new PayloadError(
      `Invalid array (wrong type ${getTypeName(value)})`,
      path
    )
  }
}

export const string: JsonPayloadTransformer<string> = {
  fromJson(input, path) {
    checkString(input, path)
    return input
  },
  toJson(output, path) {
    checkString(output, path)
    return output
  },
}

function checkString(
  value: unknown,
  path: Path
): asserts value is string {
  if (typeof value !== 'string') {
    throw new PayloadError(
      `Invalid string (wrong type ${getTypeName(value)})`,
      path
    )
  }
}

export function stringLength(config: {
  min: number
  max: number
}): JsonPayloadTransformer<string> {
  function check(value: string, path: Path): void {
    if (value.length < config.min) {
      throw new PayloadError(
        `Invalid string (too short, min ${config.min})`,
        path
      )
    } else if (value.length > config.max) {
      throw new PayloadError(
        `Invalid string (too long, max ${config.max})`,
        path
      )
    }
  }
  const base = string
  return {
    fromJson(input, path) {
      const baseInput = base.fromJson(input, path)
      check(baseInput, path)
      return baseInput
    },
    toJson(output, path) {
      check(output, path)
      const baseOutput = base.toJson(output, path)
      return baseOutput
    },
  }
}

export function stringOption<Options extends string>(
  ...options: Options[]
): JsonPayloadTransformer<Options> {
  function check(
    value: string,
    path: Path
  ): asserts value is Options {
    for (const option of options) {
      if (value === option) {
        return
      }
    }
    throw new PayloadError(
      `Invalid string option (not ${options
        .map((option) => JSON.stringify(option))
        .join(' | ')})`,
      path
    )
  }
  const base = string
  return {
    fromJson(input, path) {
      const baseInput = base.fromJson(input, path)
      check(baseInput, path)
      return baseInput
    },
    toJson(output, path) {
      check(output, path)
      const baseOutput = base.toJson(output, path)
      return baseOutput
    },
  }
}

export function stringLengthMatch(config: {
  min: number
  max: number
  match: RegExp
}): JsonPayloadTransformer<string> {
  function check(value: string, path: Path): void {
    if (!config.match.exec(value)) {
      throw new PayloadError(
        `Invalid string match (for ${JSON.stringify(
          config.match.toString()
        )})`,
        path
      )
    }
  }
  const base = stringLength(config)
  return {
    fromJson(input, path) {
      const baseInput = base.fromJson(input, path)
      check(baseInput, path)
      return baseInput
    },
    toJson(output, path) {
      check(output, path)
      const baseOutput = base.toJson(output, path)
      return baseOutput
    },
  }
}

export function stringEnum<Value extends string>(
  mapping: Record<string, Value>
): JsonPayloadTransformer<Value> {
  return stringOption(...Object.values(mapping))
}

export const number: JsonPayloadTransformer<number> = {
  fromJson(input, path) {
    checkNumber(input, path)
    return input
  },
  toJson(output, path) {
    checkNumber(output, path)
    return output
  },
}

function checkNumber(
  value: unknown,
  path: Path
): asserts value is number {
  if (typeof value !== 'number') {
    throw new PayloadError(
      `Invalid number (wrong type ${getTypeName(value)})`,
      path
    )
  }
}

export function numberRange(config: {
  min: number
  max: number
}): JsonPayloadTransformer<number> {
  function check(value: number, path: Path): void {
    if (value < config.min) {
      throw new PayloadError(
        `Invalid number (too small, min ${config.min})`,
        path
      )
    } else if (value > config.max) {
      throw new PayloadError(
        `Invalid number (too large, max ${config.max})`,
        path
      )
    }
  }
  const base = number
  return {
    fromJson(input, path) {
      const baseInput = base.fromJson(input, path)
      check(baseInput, path)
      return baseInput
    },
    toJson(output, path) {
      check(output, path)
      const baseOutput = base.toJson(output, path)
      return baseOutput
    },
  }
}

export function integerRange(config: {
  min: number
  max: number
}): JsonPayloadTransformer<number> {
  function check(value: number, path: Path): void {
    if ((value | 0) !== value) {
      throw new PayloadError(
        'Invalid integer (includes fractional component)',
        path
      )
    }
  }
  const base = numberRange(config)
  return {
    fromJson(input, path) {
      const baseInput = base.fromJson(input, path)
      check(baseInput, path)
      return baseInput
    },
    toJson(output, path) {
      check(output, path)
      const baseOutput = base.toJson(output, path)
      return baseOutput
    },
  }
}

export const boolean: JsonPayloadTransformer<boolean> = {
  fromJson(input, path) {
    checkBoolean(input, path)
    return input
  },
  toJson(output, path) {
    checkBoolean(output, path)
    return output
  },
}

function checkBoolean(
  value: unknown,
  path: Path
): asserts value is boolean {
  if (typeof value !== 'boolean') {
    throw new PayloadError(
      `Invalid boolean (wrong type ${getTypeName(value)})`,
      path
    )
  }
}

export function buildPath(
  part: string | number,
  parentPath: Path
): Path {
  return () => [...(parentPath ? parentPath() : []), part]
}

export function getTypeName(input: unknown): string {
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
