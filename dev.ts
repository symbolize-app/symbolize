import * as message from '@fe/core/message.ts'
import esbuild from 'esbuild'

console.log('DEV', esbuild, message.hi)

async function main(): Promise<void> {
  const r = await esbuild.serve(
    {},
    {
      entryPoints: ['ui/index-scratch.ts'],
      bundle: false,
      platform: 'browser',
      splitting: true,
      format: 'esm',
      outdir: 'build/browser',
      metafile: 'build/browser/meta.json',
    }
  )
  console.log(r)
}

void main()
