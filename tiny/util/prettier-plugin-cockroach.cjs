/* eslint-env node */
'use strict'

const prettier = require('prettier')
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

const lineParser = /^(?<comment>--.*$\n)|(?<blank>\s*$\n)|(?<query>[^;]*;\n)/gm

const printers = {
  'cockroach-ast': {
    /**
     * @param {prettier.FastPath<{text: string}>} path
     * @param {prettier.Options} options
     * @returns {string}
     */
    print(path, options) {
      const lines = []
      const node = path.getValue()
      for (const match of node.text.matchAll(lineParser)) {
        const { comment, blank, query } = match.groups
        if (comment) {
          lines.push(comment)
        } else if (blank !== undefined) {
          lines.push('\n')
        } else if (query) {
          const output = childProcess.execFileSync(
            'cockroach',
            [
              'sqlfmt',
              '--print-width',
              `${options.printWidth}`,
              '--tab-width',
              `${options.tabWidth}`,
              '--use-spaces',
            ],
            { encoding: 'utf-8', input: query }
          )
          lines.push(output.trimEnd())
          lines.push(';\n')
        }
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