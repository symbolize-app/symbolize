/* eslint-env node */
'use strict'

// eslint-disable-next-line @typescript-eslint/no-var-requires
const childProcess = require('child_process')

const languages = [
  {
    name: 'SQL',
    extensions: ['sql'],
    parsers: ['cockroach-parse'],
  },
]

const parsers = {
  'cockroach-parse': {
    /**
     * @param {string} text
     * @returns {{ text: string }}
     */
    parse(text) {
      return {
        text,
      }
    },
    astFormat: 'cockroach-ast',
    locStart() {
      // Unused
    },
    locEnd() {
      // Unused
    },
  },
}

const lineParser =
  /^(?<comment>--.*$\n)|(?<blank>\s*$\n)|(?<query>[^;]*;?\n)/gm

const printers = {
  'cockroach-ast': {
    /**
     * @param {import("prettier").FastPath<{text: string}>} path
     * @param {import("prettier").Options} options
     * @returns {string}
     */
    print(path, options) {
      const lines = []
      const node = path.getValue()
      let matchLength = 0
      for (const match of node.text.matchAll(lineParser)) {
        const { comment, blank, query } = match.groups ?? {}
        if (comment) {
          lines.push(comment)
          matchLength += comment.length
        } else if (blank !== undefined) {
          lines.push('\n')
          matchLength += blank.length
        } else if (query) {
          const output = childProcess
            .execFileSync(
              'cockroach',
              [
                'sqlfmt',
                '--print-width',
                `${options.printWidth ?? 0}`,
                '--tab-width',
                `${options.tabWidth ?? 0}`,
                '--use-spaces',
              ],
              { encoding: 'utf-8', input: query }
            )
            .trimEnd()
          lines.push(output)
          lines.push(';\n')
          matchLength += query.length
        }
      }
      if (matchLength !== node.text.length) {
        throw new Error('Invalid match length')
      }
      return lines.join('')
    },
  },
}

module.exports = {
  languages,
  parsers,
  printers,
}
