import type * as contrastRule from '@/rule.ts'
import prettierPostcss from 'prettier/plugins/postcss.mjs'
import * as prettier from 'prettier/standalone.mjs'

export function ruleClassNames(
  rules: readonly contrastRule.Rule[],
): readonly string[] {
  return rules.map((rule) => rule.className)
}

export async function ruleCode(
  rules: readonly contrastRule.Rule[],
): Promise<readonly string[]> {
  return Promise.all(rules.map(async (rule) => formatCode(rule.code)))
}

export async function formatCode(code: string): Promise<string> {
  return prettier.format(code, {
    parser: 'css',
    plugins: [prettierPostcss],
  })
}

export function dedent(code: string): string {
  const lines = code.split('\n')
  const validLines = lines.slice(1)
  const indent =
    (validLines[0]?.length ?? 0) - (validLines[0]?.trimStart().length ?? 0)
  const dedentedLines = validLines.map((line) => line.slice(indent))
  return dedentedLines.join('\n')
}
