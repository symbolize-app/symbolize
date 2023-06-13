/* eslint-env node */
/* eslint-disable @typescript-eslint/ban-ts-comment,@typescript-eslint/explicit-module-boundary-types,@typescript-eslint/no-unsafe-return */
import esbuild from 'esbuild'
import * as nodeFsPromises from 'node:fs/promises'
import * as nodePath from 'node:path'
import * as nodeUrl from 'node:url'
import picomatch from 'picomatch'

import projectTsconfig from '../../tsconfig.json' assert { type: 'json' }

const aliases = Object.entries(
  projectTsconfig.compilerOptions.paths
)
  .filter(
    ([_from, to]) => !to[0].startsWith('./node_modules/')
  )
  .map(([from, to]) => {
    /** @type { RegExp } */
    // @ts-ignore
    const re = picomatch.makeRe(
      from.replace('*.', '**/*.'),
      {
        // @ts-ignore
        capture: true,
      }
    )
    return (/** @type {string} */ specifier) => {
      const match = re.exec(specifier)
      if (match) {
        return to[0].replace(
          '*',
          match[1] !== undefined
            ? `${match[1]}/${match[2]}`
            : match[2]
        )
      } else {
        return undefined
      }
    }
  })

/**
 * @param {string} specifier
 * @param {{
 *   conditions: string[],
 *   parentURL: string | undefined,
 * }} context
 * @param {Function} defaultResolve
 * @returns {Promise<{
 *   format?: string
 *   url: string
 * }>}
 */
export async function resolve(
  specifier,
  context,
  defaultResolve
) {
  for (const alias of aliases) {
    const result = alias(specifier)
    if (result) {
      return { url: fromLocalPath(result) }
    }
  }

  return await defaultResolve(
    specifier,
    context,
    defaultResolve
  )
}

/**
 * @param {string} url
 * @param {{
    format?: string,
    importAssertions: Object
  }} context If resolve settled with a `format`, that value is included here.
 * @param {Function} defaultLoad
 * @returns {Promise<{
    format: string,
    source: string | ArrayBuffer | SharedArrayBuffer | Uint8Array,
  }>}
 */
export async function load(url, context, defaultLoad) {
  // Defer to Node.js for all other sources.
  const ext = nodePath.extname(url)
  const localPath = matchLocalUrl(url)
  if (localPath) {
    const source = (
      await nodeFsPromises.readFile(localPath)
    ).toString('utf-8')
    if (ext === '.sql') {
      return {
        format: 'module',
        source: `const text = ${JSON.stringify(
          source
        )}\nexport default text`,
      }
    } else {
      return {
        format: 'module',
        source: (
          await esbuild.transform(source, {
            loader: 'ts',
            define: {
              ['import.meta.env.NODE_ENV']:
                JSON.stringify('development'),
            },
          })
        ).code,
      }
    }
  } else {
    return await defaultLoad(
      url,
      {
        ...context,
        ...(ext === '.json'
          ? { importAssertions: { type: 'json' } }
          : undefined),
      },
      defaultLoad
    )
  }
}

/**
 * @param {string} url
 * @returns {string | undefined}
 */
function toLocalPath(url) {
  const urlObject = new nodeUrl.URL(url)
  if (urlObject.protocol === 'file:') {
    const otherPath = nodeUrl.fileURLToPath(urlObject)
    return nodePath.relative('.', otherPath)
  } else {
    return undefined
  }
}

/**
 * @param {string} path
 * @returns {string}
 */
function fromLocalPath(path) {
  return nodeUrl
    .pathToFileURL(nodePath.resolve('.', path))
    .toString()
}

const localMatcher = picomatch(projectTsconfig.include)

/**
 * @param {string} url
 * @returns {string | undefined}
 */
function matchLocalUrl(url) {
  const path = toLocalPath(url)
  if (path) {
    return localMatcher(path) ? path : undefined
  } else {
    return undefined
  }
}
