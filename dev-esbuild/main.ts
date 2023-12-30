#!/usr/bin/env node-loader
import * as devContext from '@/context.ts'
import * as devDatabase from '@/database.ts'
import * as devModules from '@/modules.ts'
import * as devOutput from '@/output.ts'
import * as payload from '@intertwine/lib-payload'
import * as timeNode from '@intertwine/lib-time/index.node.ts'
import chokidar from 'chokidar'
import * as esbuild from 'esbuild'
import ms from 'ms'
import * as nodeFs from 'node:fs'
import * as nodeFsPromises from 'node:fs/promises'
import * as nodePath from 'node:path'
import * as nodeUrl from 'node:url'
import * as nodeUtil from 'node:util'
import YAML from 'yaml'

const workspaceTransformer = payload.object({
  packages: payload.array(payload.string),
})

async function main(): Promise<void> {
  const args = nodeUtil.parseArgs({
    options: {
      watch: { type: 'boolean' },
      clean: { type: 'boolean' },
      mode: { type: 'string' },
    },
    strict: true,
  }).values
  const { watch, clean, mode } = {
    watch: args.watch ?? false,
    clean: args.clean ?? false,
    mode: payload
      .stringEnum(devContext.Mode)
      .fromJson(args.mode ?? devContext.Mode.development),
  }
  const outdir = nodePath.resolve(`build/guest/${mode}`)
  if (clean) {
    await nodeFsPromises.rm(outdir, {
      recursive: true,
      force: true,
    })
  } else {
    const ctx = {
      ...timeNode.initContext(),
      ...(await devDatabase.initContext()),
      outdir,
      mode,
    }
    await nodeFsPromises.mkdir(outdir, {
      recursive: true,
    })
    if (watch) {
      const workspace = workspaceTransformer.fromJson(
        YAML.parse(
          await nodeFsPromises.readFile('./pnpm-workspace.yaml', 'utf8')
        ) as payload.JsonValue
      )
      const watcher = chokidar.watch(workspace.packages, {
        ignoreInitial: true,
      })
      watcher.on('all', (type, path) => {
        console.log(`Found ${type} at ${path}`)
        void build(ctx)
      })
    }
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
    console.error(error)
  }
  if (outputFiles) {
    devOutput.write(
      ctx,
      versionId,
      outputFiles.map((item) => ({
        pathId: item.path.substring(ctx.outdir.length + 1),
        original: item.contents,
      }))
    )
  } else {
    process.exitCode = 1
  }
  const end = ctx.time.performanceNow()
  const elapsed = ms(Math.round(end - start))
  console.log(`Done build ${versionId}: ${elapsed}`)
}

async function buildFiles(
  ctx: devContext.Context
): Promise<devModules.BuildResult> {
  const classicEntryPoints = [
    './svc-gateway-guest-run/init.html',
    './svc-gateway-guest-run/main.html',
    './svc-gateway-guest-run/serviceWorker.ts',
  ]
  const moduleEntryPoints = [
    './svc-gateway-guest-run/dedicatedWorker.ts',
    './svc-gateway-guest-run/main.ts',
    './svc-gateway-guest-run/serviceWorkerRegister.ts',
  ]
  const commonOptions = {
    platform: 'browser' as const,
    outdir: ctx.outdir,
    outbase: nodePath.resolve('.'),
    define: {
      ['import.meta.env.NODE_ENV']: JSON.stringify(ctx.mode),
    },
    logLevel: 'warning' as const,
    minify: ctx.mode === devContext.Mode.production,
    write: false,
    external: ['timers', 'util'],
  }
  const classicResultPromise = esbuild.build({
    ...commonOptions,
    format: 'iife',
    bundle: true,
    entryPoints: classicEntryPoints.map((entryPoint) =>
      nodePath.resolve(entryPoint)
    ),
    loader: {
      ['.html']: 'copy',
    },
    outExtension: {
      ['.js']: '.ts.js',
    },
  })
  const moduleResultPromise = devModules.build({
    ...commonOptions,
    format: 'esm',
    entryPoints: moduleEntryPoints.map((entryPoint) =>
      nodePath.resolve(entryPoint)
    ),
  })
  const [classicResult, moduleResult] = await Promise.all([
    classicResultPromise,
    moduleResultPromise,
  ])
  return {
    errors: [...classicResult.errors, ...moduleResult.errors],
    warnings: [...classicResult.warnings, ...moduleResult.warnings],
    outputFiles: [
      ...(classicResult.outputFiles ?? []),
      ...moduleResult.outputFiles,
    ],
  }
}

if (
  process.argv[1] &&
  nodeFs.realpathSync(process.argv[1]) ===
    nodeUrl.fileURLToPath(import.meta.url)
) {
  void main()
}
