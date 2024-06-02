/* eslint-env node */
import esbuild from 'esbuild'
import * as nodePath from 'node:path'
import * as nodeUrl from 'node:url'

/**
 * @typedef {import("node:module").LoadHook} LoadHook
 */

/**
 * @type {Readonly<RegExp>}
 */
const localMatcher = /^node:|\/node_modules\//

/**
 * @type {LoadHook}
 */
// eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types, @typescript-eslint/explicit-function-return-type, @typescript-eslint/promise-function-async, functional/prefer-immutable-types
export const load = (url, context, defaultLoad) => {
  // Defer to Node.js for all other sources.
  const ext = nodePath.extname(url)
  if (!localMatcher.exec(url)) {
    const fullPath = nodeUrl.fileURLToPath(new nodeUrl.URL(url))
    return (async () => {
      const result = await esbuild.build({
        absWorkingDir: nodePath.dirname(fullPath),
        bundle: true,
        entryPoints: [fullPath],
        external: ['/*', 'node:*'],
        format: 'esm',
        loader: {
          ['.sql']: 'text',
        },
        logLevel: 'warning',
        platform: 'node',
        target: [`node${process.versions.node}`],
        write: false,
      })
      const outputFile = result.outputFiles[0]
      if (!outputFile) {
        throw new Error('Missing output file')
      }
      return {
        format: 'module',
        shortCircuit: true,
        source: outputFile.contents,
      }
    })()
  } else {
    return defaultLoad(url, {
      ...context,
      ...(ext === '.json' ?
        { importAssertions: { type: 'json' } }
      : undefined),
    })
  }
}
