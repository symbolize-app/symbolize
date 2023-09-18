import * as buildModules from '@intertwine/dev-build/modules.ts'
import * as nodeFsPromises from 'node:fs/promises'
import * as nodePath from 'node:path'
import * as nodeUrl from 'node:url'

async function main(): Promise<void> {
  await nodeFsPromises.rm('build', {
    recursive: true,
    force: true,
  })
  await nodeFsPromises.mkdir('build/browser', {
    recursive: true,
  })
  await nodeFsPromises.copyFile(
    'svc-gateway-guest/public/index.html',
    'build/browser/index.html'
  )
  await buildModules.build({
    entryPoints: [
      nodePath.resolve('./svc-auth-guest-display/index.ts'),
    ],
    format: 'esm',
    platform: 'browser',
    outdir: nodePath.resolve('build/browser/js'),
    outbase: nodePath.resolve('.'),
    define: {
      ['import.meta.env.NODE_ENV']:
        JSON.stringify('production'),
    },
    logLevel: 'info',
  })
}

if (
  process.argv[1] === nodeUrl.fileURLToPath(import.meta.url)
) {
  void main()
}
