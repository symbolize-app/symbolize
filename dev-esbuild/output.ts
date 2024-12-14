import * as devContext from '@/context.ts'
import * as collection from '@symbolize/lib-collection'
import * as hex from '@symbolize/lib-hex'
import * as nodeCrypto from 'node:crypto'
import * as nodePath from 'node:path'
import * as nodeZlib from 'node:zlib'

class OutputFile {
  constructor(
    readonly original: Uint8Array,
    readonly pathId: string,
  ) {}
}

class ContentFile {
  constructor(
    readonly contentId: Uint8Array,
    readonly original: Uint8Array,
    readonly pathId: string,
  ) {}
}

const serviceWorkerMainPath = 'svc-gateway-guest-run/serviceWorker.ts.js'
const serviceWorkerShellPath =
  'svc-gateway-guest-run/serviceWorkerShell.js'

export function createVersion(): bigint {
  return BigInt(Date.now())
}

export function write(
  ctx: devContext.Context,
  versionId: bigint,
  outputFiles: readonly OutputFile[],
): void {
  const mainContentFiles = outputFiles.map(convertToContentFile)
  const manifestFiles = collection
    .groupBy(mainContentFiles, (item) =>
      item.pathId.substring(0, item.pathId.indexOf('/')),
    )
    .map(([name, items]) => buildManifest(name, items))
    .map(convertToContentFile)
  const serviceWorkerShell = convertToContentFile(
    buildServiceWorkerShell(versionId, mainContentFiles, manifestFiles),
  )
  const allContentFiles = [
    ...mainContentFiles,
    ...manifestFiles,
    serviceWorkerShell,
  ]

  for (const contentFile of allContentFiles) {
    const contentResult = ctx.db.query.upsertContent.run({
      id: contentFile.contentId,
      original: contentFile.original,
    })
    if (
      contentResult.lastInsertRowid &&
      ctx.dev.mode === devContext.Mode.production
    ) {
      const compressed = compressContent(contentFile.original)
      ctx.db.query.updateContentCompressed.run({
        compressed,
        id: contentFile.contentId,
      })
    }
  }

  ctx.db.withTransactionSync(() => {
    ctx.db.query.insertVersion.run({ id: versionId })
    for (const contentFile of allContentFiles) {
      ctx.db.query.insertPath.run({
        content_id: contentFile.contentId,
        id: contentFile.pathId,
        version_id: versionId,
      })
    }
  })
}

function convertToContentFile(outputFile: OutputFile): ContentFile {
  return new ContentFile(
    getContentId(outputFile.original),
    outputFile.original,
    outputFile.pathId,
  )
}

function getContentId(original: Uint8Array): Uint8Array {
  const hash = nodeCrypto.createHash('sha256')
  hash.update(original)
  return hash.digest()
}

function buildManifest(
  name: string,
  contentFiles: readonly ContentFile[],
): OutputFile {
  const data = Object.fromEntries(
    contentFiles.map((item) => [
      item.pathId,
      `${hex.uint8ArrayToHex(item.contentId)}${nodePath.extname(
        item.pathId,
      )}`,
    ]),
  )
  const pathId = `.manifest/${name}.js`
  const text = `Object.assign(manifest,${JSON.stringify(data)})`
  const original = Buffer.from(text, 'utf8')
  return new OutputFile(original, pathId)
}

function buildServiceWorkerShell(
  versionId: bigint,
  mainContentFiles: readonly ContentFile[],
  manifestFiles: readonly ContentFile[],
): OutputFile {
  const serviceWorkerMain = mainContentFiles.find(
    (item) => item.pathId === serviceWorkerMainPath,
  )
  if (!serviceWorkerMain) {
    throw new Error('Service worker main not found')
  }
  const scripts = [...manifestFiles, serviceWorkerMain]
    .map(
      (item) => `"/.code/.id/${hex.uint8ArrayToHex(item.contentId)}.js"`,
    )
    .join(',')
  const pathId = serviceWorkerShellPath
  const text = [
    `version=BigInt(${versionId})`,
    'manifest={}',
    `importScripts(${scripts})`,
  ].join(';')
  const original = Buffer.from(text, 'utf8')
  return { original, pathId }
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
