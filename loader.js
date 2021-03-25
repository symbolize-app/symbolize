/* eslint-env node */
/* eslint-disable @typescript-eslint/ban-ts-comment,@typescript-eslint/explicit-module-boundary-types,@typescript-eslint/no-unsafe-return */
import esbuild from 'esbuild'
import * as pathModule from 'path'
import picomatch from 'picomatch'
import * as urlModule from 'url'

import tsconfig from './tsconfig.json'

const aliases = Object.entries(
  tsconfig.compilerOptions.paths
).map(([from, to]) => {
  /** @type { RegExp } */
  // @ts-ignore
  const re = picomatch.makeRe(from.replace('*.', '**/*.'), {
    // @ts-ignore
    capture: true,
  })
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
 *   conditions: !Array<string>,
 *   parentURL: !(string | undefined),
 * }} context
 * @param {Function} defaultResolve
 * @returns {{ url: string }}
 */
export function resolve(
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

  return defaultResolve(specifier, context, defaultResolve)
}

/**
 * @param {string} url
 * @param {Object} context (currently empty)
 * @param {Function} defaultGetFormat
 * @returns {{ format: string }}
 */
export function getFormat(url, context, defaultGetFormat) {
  if (isLocalUrl(url)) {
    return {
      format: 'module',
    }
  } else {
    return defaultGetFormat(url, context, defaultGetFormat)
  }
}

/**
 * @param {!(string | SharedArrayBuffer | Uint8Array)} source
 * @param {{
 *   format: string,
 *   url: string,
 * }} context
 * @param {Function} defaultTransformSource
 * @returns {Promise<{ source: !(string | SharedArrayBuffer | Uint8Array) }>}
 */
export async function transformSource(
  source,
  context,
  defaultTransformSource
) {
  // Defer to Node.js for all other sources.
  if (isLocalUrl(context.url)) {
    if (typeof source !== 'string') {
      source = Buffer.from(source).toString('utf-8')
    }
    const ext = pathModule.extname(context.url)
    if (ext === '.sql') {
      return {
        source: `const text = ${JSON.stringify(
          source
        )}\nexport default text`,
      }
    } else {
      return {
        source: (
          await esbuild.transform(source, {
            loader: 'ts',
            define: {
              ['import.meta.env.NODE_ENV']: JSON.stringify(
                'development'
              ),
            },
          })
        ).code,
      }
    }
  } else {
    return defaultTransformSource(
      source,
      context,
      defaultTransformSource
    )
  }
}

/**
 * @param {string} url
 * @returns {string | undefined}
 */
function toLocalPath(url) {
  const urlObject = new urlModule.URL(url)
  if (urlObject.protocol === 'file:') {
    const otherPath = urlModule.fileURLToPath(urlObject)
    return pathModule.relative('.', otherPath)
  } else {
    return undefined
  }
}

/**
 * @param {string} path
 * @returns {string}
 */
function fromLocalPath(path) {
  return urlModule
    .pathToFileURL(pathModule.resolve('.', path))
    .toString()
}

const localMatcher = picomatch(tsconfig.include)

/**
 * @param {string} url
 * @returns {boolean}
 */
function isLocalUrl(url) {
  const path = toLocalPath(url)
  if (path) {
    return localMatcher(path)
  } else {
    return false
  }
}
