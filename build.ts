import esbuild from 'esbuild'
import fs from 'fs/promises'
import module from 'module'
import pathModule from 'path'
import urlModule from 'url'

async function main(): Promise<void> {
  await fs.rm('build', { recursive: true, force: true })
  await fs.mkdir('build/browser', { recursive: true })
  await fs.copyFile(
    'public/index.html',
    'build/browser/index.html'
  )
  const completedSteps: Set<string> = new Set()
  const nextSteps: string[] = [
    await import.meta.resolve('@fe/ui/index.ts'),
  ]
  // eslint-disable-next-line no-constant-condition
  while (true) {
    const step = nextSteps.pop()
    if (!step) {
      break
    } else if (completedSteps.has(step)) {
      continue
    }
    const result = await build({ entryPoint: step })
    completedSteps.add(step)
    nextSteps.push(...result.nextSteps)
  }
  console.log('Done build')
}

type BuildOptions = {
  entryPoint: string
}

type BuildResult = {
  nextSteps: string[]
}

const localRootPath = pathModule.resolve(
  urlModule.fileURLToPath(import.meta.url),
  '..'
)

async function build(
  options: BuildOptions
): Promise<BuildResult> {
  const fullEntryPointPath = urlModule.fileURLToPath(
    options.entryPoint
  )
  const localEntryPointPath = pathModule.relative(
    localRootPath,
    fullEntryPointPath
  )
  const outfile = pathModule
    .join('build/browser/js', localEntryPointPath)
    .replace(/\.[^.]+$/, '.js')
  const nextSteps: string[] = []
  await esbuild.build({
    bundle: true,
    define: {
      ['import.meta.env.MODE']: JSON.stringify(
        'development'
      ),
    },
    entryPoints: [fullEntryPointPath],
    format: 'esm',
    outfile,
    platform: 'browser',
    plugins: [
      {
        name: 'custom',
        setup: plugin,
      },
    ],
  })
  return { nextSteps }

  function plugin(build: esbuild.PluginBuild) {
    build.onResolve({ filter: /.*/ }, async (args) => {
      if (args.kind === 'entry-point') {
        return undefined
      } else if (
        args.kind === 'import-statement' ||
        args.kind === 'dynamic-import'
      ) {
        const url = await import.meta.resolve(
          args.path,
          urlModule.pathToFileURL(args.importer).toString()
        )
        if (url.startsWith('node:')) {
          return { path: args.path, external: true }
        }
        const fullPath = urlModule.fileURLToPath(url)
        const localPath = pathModule.relative(
          localRootPath,
          fullPath
        )
        if (localPath.startsWith(`..${pathModule.sep}`)) {
          throw new Error(`Invalid path ${args.path}`)
        } else {
          const path = pathModule
            .join('/js', localPath)
            .replace(/\.[^.]+$/, '.js')
          nextSteps.push(url)
          return { path, external: true }
        }
      } else {
        const fullPath = module
          .createRequire(args.importer)
          .resolve(args.path)
        if (!pathModule.isAbsolute(fullPath)) {
          return {
            path: args.path,
            namespace: 'external',
          }
        }
        return { path: fullPath, namespace: 'file' }
      }
    })
  }
}

void main()
