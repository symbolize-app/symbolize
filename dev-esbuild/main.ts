#!/usr/bin/env node-loader
import * as devDatabase from '@/database.ts'
import * as devModules from '@/modules.ts'
import * as payload from '@intertwine/lib-payload'
import type * as time from '@intertwine/lib-time'
import * as timeNode from '@intertwine/lib-time/index.node.ts'
import chokidar from 'chokidar'
import * as esbuild from 'esbuild'
import lodashDebounce from 'lodash-es/debounce.js'
import ms from 'ms'
import * as nodeCrypto from 'node:crypto'
import * as nodeFs from 'node:fs'
import * as nodeFsPromises from 'node:fs/promises'
import * as nodePath from 'node:path'
import * as nodeUrl from 'node:url'
import * as nodeUtil from 'node:util'
import * as nodeZlib from 'node:zlib'
import YAML from 'yaml'

type Context = time.Context & devDatabase.Context

enum Mode {
  development = 'development',
  production = 'production',
}

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
    const ctx = {
      ...timeNode.initContext(),
      ...(await devDatabase.initContext()),
    }
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
    ctx.db.close()
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
  const versionId = createVersion(ctx)
  const { errors, outputFiles } = await buildFiles(options)
  if (!errors.length) {
    writeOutputFiles(ctx, options, versionId, outputFiles)
  }
  const end = ctx.performanceNow()
  const elapsed = ms(Math.round(end - start))
  console.log(`Done build: ${elapsed}`)
}

function createVersion(ctx: Context): bigint {
  const versionId = BigInt(Date.now())
  ctx.query.insertVersion.run({ id: versionId })
  return versionId
}

async function buildFiles(options: {
  outdir: string
  mode: Mode
}): Promise<devModules.BuildResult> {
  const classicEntryPoints = [
    './svc-gateway-guest-run/serviceWorker.ts',
  ]
  const moduleEntryPoints = [
    './svc-auth-guest-view/main.ts',
    './svc-gateway-guest-run/serviceWorkerRegister.ts',
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
    outExtension: { ['.js']: '.ts.js' },
  })
  const moduleResultPromise = devModules.build({
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

function writeOutputFiles(
  ctx: Context,
  options: {
    outdir: string
    mode: Mode
  },
  versionId: bigint,
  outputFiles: esbuild.OutputFile[]
): void {
  for (const outputFile of outputFiles) {
    const pathId = outputFile.path.substring(
      options.outdir.length
    )
    const original = outputFile.contents
    const contentId = getContentId(original)
    const contentResult = ctx.query.upsertContent.run({
      id: contentId,
      original,
    })
    if (
      contentResult.lastInsertRowid &&
      options.mode === Mode.production
    ) {
      const compressed = compressContent(original)
      ctx.query.updateContentCompressed.run({
        id: contentId,
        compressed,
      })
    }
    ctx.query.insertPath.run({
      id: pathId,
      version_id: versionId,
      content_id: contentId,
    })
  }
}

function getContentId(original: Uint8Array): Uint8Array {
  const hash = nodeCrypto.createHash('sha256')
  hash.update(original)
  return hash.digest()
}

function compressContent(original: Uint8Array): Uint8Array {
  return nodeZlib.brotliCompressSync(original, {
    params: {
      [nodeZlib.constants.BROTLI_PARAM_MODE]:
        nodeZlib.constants.BROTLI_MODE_TEXT,
      [nodeZlib.constants.BROTLI_PARAM_QUALITY]:
        nodeZlib.constants.BROTLI_MAX_QUALITY,
    },
  })
}

if (
  nodeFs.realpathSync(process.argv[1]) ===
  nodeUrl.fileURLToPath(import.meta.url)
) {
  void main()
}
