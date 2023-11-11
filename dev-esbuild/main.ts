#!/usr/bin/env node-loader
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
import * as z from 'zod'

import * as modules from '@/modules.ts'

export type Context = tinyTime.Context

const modeSchema = z.enum(['development', 'production'])
type Mode = z.infer<typeof modeSchema>

const argsSchema = z.object({
  watch: z.boolean().default(false),
  clean: z.boolean().default(false),
  mode: modeSchema.default(modeSchema.enum.development),
})

const workspaceSchema = z.object({
  packages: z.string().array(),
})

async function main(): Promise<void> {
  const ctx = tinyTimeNode.initContext()
  const { watch, clean, mode } = argsSchema.parse(
    nodeUtil.parseArgs({
      options: {
        watch: { type: 'boolean' },
        clean: { type: 'boolean' },
        mode: { type: 'string' },
      },
      strict: true,
    }).values
  )
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
      const workspace = workspaceSchema.parse(
        YAML.parse(
          await nodeFsPromises.readFile(
            './pnpm-workspace.yaml',
            'utf8'
          )
        )
      )
      const watcher = chokidar.watch(workspace.packages, {
        ignoreInitial: true,
      })
      watcher.on(
        'all',
        lodashDebounce((type, path) => {
          console.log(`Found ${type} at ${path}`)
          void buildCommon(ctx, {
            outdir,
            mode,
          })
        })
      )
    }
    await buildCommon(ctx, {
      outdir,
      mode,
    })
  }
}

export type { BuildResult } from '@/modules.ts'

export async function buildCommon(
  ctx: Context,
  options: {
    outdir: string
    mode: Mode
  }
): Promise<modules.BuildResult> {
  const start = ctx.performanceNow()
  const classicEntryPoints = [
    './svc-gateway-guest/serviceWorker.ts',
  ]
  const moduleEntryPoints = [
    './svc-auth-guest-display/index.ts',
    './svc-gateway-guest/serviceWorkerRegister.ts',
  ]
  const commonOptions = {
    format: 'esm' as const,
    platform: 'browser' as const,
    outdir: options.outdir,
    outbase: nodePath.resolve('.'),
    define: {
      ['import.meta.env.NODE_ENV']: JSON.stringify(
        options.mode
      ),
    },
    logLevel: 'warning' as const,
    write: true,
    external: ['timers', 'util'],
  }
  const classicResultPromise = esbuild.build({
    ...commonOptions,
    bundle: true,
    entryPoints: classicEntryPoints.map((entryPoint) =>
      nodePath.resolve(entryPoint)
    ),
    outExtension: { ['.js']: '.ts.js' },
  })
  const moduleResultPromise = modules.build({
    ...commonOptions,
    entryPoints: moduleEntryPoints.map((entryPoint) =>
      nodePath.resolve(entryPoint)
    ),
  })
  const classicResult = await classicResultPromise
  const moduleResult = await moduleResultPromise
  const end = ctx.performanceNow()
  const elapsed = ms(Math.round(end - start))
  console.log(`Done build: ${elapsed}`)
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
