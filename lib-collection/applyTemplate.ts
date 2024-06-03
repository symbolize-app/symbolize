export function applyTemplate(
  parameterPattern: Readonly<RegExp>,
  template: string,
  parameters: Record<string, string>,
): string {
  return template.replace(
    parameterPattern,
    (_match, parameterName: string) => {
      const parameterResult = parameters[parameterName]
      if (parameterResult === undefined) {
        throw new Error(`Parameter ${parameterName} not defined`)
      } else {
        return parameterResult
      }
    },
  )
}
