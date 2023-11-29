#!/usr/bin/env node-loader
import * as payload from '@intertwine/lib-payload/index.ts'
import * as tinyTimeNode from '@intertwine/lib-time/time.node.ts'
import type * as tinyTime from '@intertwine/lib-time/time.ts'
import chokidar from 'chokidar'
import * as esbuild from 'esbuild'
import lodashDebounce from 'lodash-es/debounce.js'
import ms from 'ms'
import * as nodeFs from 'node:fs'
import * as nodeFsPromises from 'node:fs/promises'
import * as nodePath from 'node:path'
import * as nodeUrl from 'node:url'
import * as nodeUtil from 'node:util'
import YAML from 'yaml'

import * as modules from '@/modules.ts'

type Context = tinyTime.Context

enum Mode {
  development = 'development',
  production = 'production',
}

const workspaceTransformer = payload.object({
  packages: payload.array(payload.string),
})

async function main(): Promise<void> {
  const ctx = tinyTimeNode.initContext()
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
      .stringEnum(Mode)
      .fromJson(args.mode ?? Mode.development),
  }
  const outdir = nodePath.resolve(`build/guest/${mode}`)
  if (clean) {
    await nodeFsPromises.rm(outdir, {
      recursive: true,
      force: true,
    })
  } else {
    await nodeFsPromises.mkdir(outdir, {
      recursive: true,
    })
    if (watch) {
      const workspace = workspaceTransformer.fromJson(
        YAML.parse(
          await nodeFsPromises.readFile(
            './pnpm-workspace.yaml',
            'utf8'
          )
        ) as payload.JsonValue
      )
      const watcher = chokidar.watch(workspace.packages, {
        ignoreInitial: true,
      })
      watcher.on(
        'all',
        lodashDebounce((type, path) => {
          console.log(`Found ${type} at ${path}`)
          void build(ctx, {
            outdir,
            mode,
          })
        })
      )
    }
    await build(ctx, {
      outdir,
      mode,
    })
  }
}

async function build(
  ctx: Context,
  options: {
    outdir: string
    mode: Mode
  }
): Promise<void> {
  const start = ctx.performanceNow()
  await buildFiles(options)
  const end = ctx.performanceNow()
  const elapsed = ms(Math.round(end - start))
  console.log(`Done build: ${elapsed}`)
}

async function buildFiles(options: {
  outdir: string
  mode: Mode
}): Promise<modules.BuildResult> {
  const classicEntryPoints = [
    './svc-gateway-guest/serviceWorker.ts',
  ]
  const moduleEntryPoints = [
    './svc-auth-guest-display/index.ts',
    './svc-gateway-guest/serviceWorkerRegister.ts',
  ]
  const commonOptions = {
    platform: 'browser' as const,
    outdir: options.outdir,
    outbase: nodePath.resolve('.'),
    define: {
      ['import.meta.env.NODE_ENV']: JSON.stringify(
        options.mode
      ),
    },
    logLevel: 'warning' as const,
    minify: options.mode === Mode.production,
    write: true,
    external: ['timers', 'util'],
  }
  const classicResultPromise = esbuild.build({
    ...commonOptions,
    format: 'iife',
    bundle: true,
    entryPoints: classicEntryPoints.map((entryPoint) =>
      nodePath.resolve(entryPoint)
    ),
    outExtension: { ['.js']: '.ts.js' },
  })
  const moduleResultPromise = modules.build({
    ...commonOptions,
    format: 'esm',
    entryPoints: moduleEntryPoints.map((entryPoint) =>
      nodePath.resolve(entryPoint)
    ),
  })
  const classicResult = await classicResultPromise
  const moduleResult = await moduleResultPromise
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
  nodeFs.realpathSync(process.argv[1]) ===
  nodeUrl.fileURLToPath(import.meta.url)
) {
  void main()
}
