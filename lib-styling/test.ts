import * as stylingAtom from '@/atom.ts'
import type * as stylingContext from '@/context.ts'
import type * as stylingRule from '@/rule.ts'
import * as test from '@symbolize/lib-test'
import prettierPostcss from 'prettier/plugins/postcss.mjs'
import * as prettier from 'prettier/standalone.mjs'

export async function testCompile(
  ctx: stylingContext.Context,
  atomOpt: stylingAtom.AtomOpt,
): Promise<{
  readonly classNames: readonly string[]
  readonly code: readonly string[]
}> {
  const rules = [...stylingAtom.compile(ctx, atomOpt)]
  return {
    classNames: ruleClassNames(rules),
    code: await ruleCode(rules),
  }
}

export async function assertCompileBasicEquals(
  ctx: stylingContext.Context,
  atomOpt: stylingAtom.AtomOpt,
  code: string,
): Promise<void> {
  const result = await testCompile(ctx, atomOpt)

  test.assertDeepEquals(result.classNames, ['a0'])
  test.assertDeepEquals(result.code, [
    dedent(`
      .a0 {
        ${code};
      }
    `),
  ])
}

function ruleClassNames(
  rules: readonly stylingRule.Rule[],
): readonly string[] {
  return rules.map((rule) => rule.className)
}

async function ruleCode(
  rules: readonly stylingRule.Rule[],
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
