import * as esbuild from 'esbuild'
import * as nodeFsPromises from 'node:fs/promises'
import * as nodePath from 'node:path'
import * as nodeUrl from 'node:url'

import * as modules from '@/modules.ts'

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
  await buildCommon({
    outdir: nodePath.resolve('build/browser/js'),
    nodeEnv: 'production',
    write: true,
  })
}

export type { BuildResult } from '@/modules.ts'

export async function buildCommon(options: {
  outdir: string
  nodeEnv: string
  write: boolean
}): Promise<modules.BuildResult> {
  const classicEntryPoints = [
    './svc-gateway-guest/serviceWorker.ts',
  ]
  const moduleEntryPoints = [
    './svc-auth-guest-display/index.ts',
  ]
  const commonOptions = {
    format: 'esm' as const,
    platform: 'browser' as const,
    outdir: options.outdir,
    outbase: nodePath.resolve('.'),
    define: {
      ['import.meta.env.NODE_ENV']: JSON.stringify(
        options.nodeEnv
      ),
    },
    logLevel: 'warning' as const,
    write: options.write,
    external: ['timers', 'util'],
  }
  const classicResultPromise = esbuild.build({
    ...commonOptions,
    bundle: true,
    entryPoints: classicEntryPoints.map((entryPoint) =>
      nodePath.resolve(entryPoint)
    ),
    outExtension: { ['.js']: '.mjs' },
  })
  const moduleResultPromise = modules.build({
    ...commonOptions,
    entryPoints: moduleEntryPoints.map((entryPoint) =>
      nodePath.resolve(entryPoint)
    ),
  })
  const classicResult = await classicResultPromise
  const moduleResult = await moduleResultPromise
  // TODO Print build time
  return {
    errors: [
      ...classicResult.errors,
      ...moduleResult.errors,
    ],
    warnings: [
      ...classicResult.warnings,
      ...moduleResult.warnings,
    ],
    outputFiles: [
      ...(classicResult.outputFiles ?? []),
      ...moduleResult.outputFiles,
    ],
  }
}

if (
  process.argv[1] === nodeUrl.fileURLToPath(import.meta.url)
) {
  void main()
}
