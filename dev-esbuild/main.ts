#!/usr/bin/env node-loader
import * as devContext from '@/context.ts'
import * as devDatabase from '@/db.ts'
import * as devModules from '@/modules.ts'
import * as devOutput from '@/output.ts'
import * as payload from '@intertwine/lib-payload'
import * as time from '@intertwine/lib-time'
import * as esbuild from 'esbuild'
import ms from 'ms'
import * as nodeFs from 'node:fs'
import * as nodeFsPromises from 'node:fs/promises'
import * as nodePath from 'node:path'
import * as nodeUrl from 'node:url'
import * as nodeUtil from 'node:util'

async function main(): Promise<void> {
  const args = nodeUtil.parseArgs({
    options: {
      clean: { type: 'boolean' },
      mode: { type: 'string' },
    },
    strict: true,
  }).values
  const { clean, mode } = {
    clean: args.clean ?? false,
    mode: payload
      .stringEnum(devContext.Mode)
      .fromJson(args.mode ?? devContext.Mode.development),
  }
  const outdir = nodePath.resolve(`build/guest/${mode}`)
  if (clean) {
    await nodeFsPromises.rm(outdir, {
      force: true,
      recursive: true,
    })
  } else {
    const ctx = {
      db: await devDatabase.db(),
      dev: devContext.dev(mode, outdir),
      time: time.time(),
    }
    await nodeFsPromises.mkdir(outdir, {
      recursive: true,
    })
    await build(ctx)
  }
}

async function build(ctx: devContext.Context): Promise<void> {
  const start = ctx.time.performanceNow()
  const versionId = devOutput.createVersion()
  let outputFiles = null
  try {
    const buildResult = await buildFiles(ctx)
    if (!buildResult.errors.length) {
      outputFiles = buildResult.outputFiles
    }
  } catch (error) {
    // eslint-disable-next-line no-console
    console.error(error)
  }
  if (outputFiles) {
    devOutput.write(
      ctx,
      versionId,
      outputFiles.map((item) => ({
        original: item.contents,
        pathId: item.path.substring(ctx.dev.outdir.length + 1),
      })),
    )
  } else {
    // eslint-disable-next-line functional/immutable-data -- main API
    process.exitCode = 1
  }
  const end = ctx.time.performanceNow()
  const elapsed = ms(Math.round(end - start))
  // eslint-disable-next-line no-console
  console.log(`Done build ${versionId}: ${elapsed}`)
}

async function buildFiles(
  ctx: devContext.Context,
): Promise<devModules.BuildResult> {
  const copyEntryPoints = [
    './svc-gateway-guest-run/.font/literata-italic.woff2',
    './svc-gateway-guest-run/.font/literata.woff2',
    './svc-gateway-guest-run/init.html',
  ]
  const classicEntryPoints = ['./svc-gateway-guest-run/serviceWorker.ts']
  const moduleEntryPoints = [
    './svc-gateway-guest-run/dedicatedWorker.ts',
    './svc-gateway-guest-run/main.ts',
    './svc-gateway-guest-run/serviceWorkerRegister.ts',
  ]
  const commonOptions = {
    define: { ['import.meta.env.NODE_ENV']: JSON.stringify(ctx.dev.mode) },
    external: ['timers', 'util'],
    loader: {
      ['.css']: 'text',
      ['.html']: 'text',
      ['.txt']: 'text',
    } as const,
    logLevel: 'warning' as const,
    minify: ctx.dev.mode === devContext.Mode.production,
    outbase: nodePath.resolve('.'),
    outdir: ctx.dev.outdir,
    platform: 'browser' as const,
    write: false,
  }
  const copyResultPromise = esbuild.build({
    ...commonOptions,
    bundle: true,
    entryPoints: copyEntryPoints.map((entryPoint) =>
      nodePath.resolve(entryPoint),
    ),
    loader: {
      ['.html']: 'copy',
      ['.woff2']: 'copy',
    },
  })
  const classicResultPromise = esbuild.build({
    ...commonOptions,
    bundle: true,
    entryPoints: classicEntryPoints.map((entryPoint) =>
      nodePath.resolve(entryPoint),
    ),
    format: 'iife',
    outExtension: {
      ['.js']: '.ts.js',
    },
  })
  const moduleResultPromise = devModules.build({
    ...commonOptions,
    entryPoints: moduleEntryPoints.map((entryPoint) =>
      nodePath.resolve(entryPoint),
    ),
    format: 'esm',
  })
  const [copyResult, classicResult, moduleResult] = await Promise.all([
    copyResultPromise,
    classicResultPromise,
    moduleResultPromise,
  ])
  return {
    errors: [...classicResult.errors, ...moduleResult.errors],
    outputFiles: [
      ...(copyResult.outputFiles ?? []),
      ...(classicResult.outputFiles ?? []),
      ...moduleResult.outputFiles,
    ],
    warnings: [...classicResult.warnings, ...moduleResult.warnings],
  }
}

if (
  process.argv[1] &&
  nodeFs.realpathSync(process.argv[1]) ===
    nodeUrl.fileURLToPath(import.meta.url)
) {
  void main()
}
