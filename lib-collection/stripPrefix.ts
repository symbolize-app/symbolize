export function stripPrefix(
  input: string,
  pattern: RegExp
): string | null {
  const match = pattern.exec(input)
  if (match) {
    return input.substring(match[0].length)
  } else {
    return null
  }
}
