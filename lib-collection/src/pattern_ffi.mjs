import { Result$Error, Result$Ok } from './gleam.mjs'

export function new_pattern(source, flags) {
  return new RegExp(source, flags)
}

export function exec_substring(pattern, input) {
  const match = pattern.exec(input)
  return [Boolean(match), match ? input.substring(match[0].length) : '']
}

export function same_key(first, second) {
  return first === second || (first !== first && second !== second)
}

export function replace_template(pattern, input, lookup) {
  try {
    const output = input.replace(pattern, (_match, parameterName) => {
      const result = lookup(parameterName)
      if (result.isOk()) return result[0]
      throw result
    })
    return Result$Ok(output)
  } catch (error) {
    if (error && typeof error.isOk === 'function' && !error.isOk()) {
      return Result$Error(error[0])
    }
    throw error
  }
}
