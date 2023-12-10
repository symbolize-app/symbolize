/* eslint-env node */
import esbuild from 'esbuild'
import * as nodeFsPromises from 'node:fs/promises'
import * as nodePath from 'node:path'
import * as nodeUrl from 'node:url'

/**
 * @typedef {import("node:module").LoadHook} LoadHook
 */

/**
 * @type {RegExp}
 */
const localMatcher = /^node:|\/node_modules\//

/**
 * @type {LoadHook}
 */
export const load = (url, context, defaultLoad) => {
  // Defer to Node.js for all other sources.
  const ext = nodePath.extname(url)
  if (!localMatcher.exec(url)) {
    const fullPath = nodeUrl.fileURLToPath(new nodeUrl.URL(url))
    return (async () => {
      if (ext === '.sql') {
        const source = (await nodeFsPromises.readFile(fullPath)).toString(
          'utf-8'
        )
        return {
          format: 'module',
          source: `const text = ${JSON.stringify(
            source
          )}\nexport default text`,
          shortCircuit: true,
        }
      } else {
        const result = await esbuild.build({
          entryPoints: [fullPath],
          absWorkingDir: nodePath.dirname(fullPath),
          bundle: true,
          write: false,
          format: 'esm',
          platform: 'node',
          target: [`node${process.versions.node}`],
          external: ['/*', 'node:*'],
          logLevel: 'warning',
        })
        return {
          format: 'module',
          source: result.outputFiles[0].contents,
          shortCircuit: true,
        }
      }
    })()
  } else {
    return defaultLoad(url, {
      ...context,
      ...(ext === '.json'
        ? { importAssertions: { type: 'json' } }
        : undefined),
    })
  }
}
