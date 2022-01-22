import * as appDocumentType from '@app/core/documentType.ts'
import * as appLanguage from '@app/core/language.ts'
import * as appTaxonRank from '@app/core/taxonRank.ts'
import * as tinyPayload from '@tiny/core/payload.ts'

export const content = tinyPayload.string({
  min: 1,
  max: 4096,
})

export const documentType = tinyPayload.stringEnum(
  appDocumentType.DocumentType
)

export const email = tinyPayload.string({
  min: 5,
  max: 64,
})

export const handle = tinyPayload.string({
  min: 3,
  max: 64,
})

export const id = tinyPayload.string({
  min: 64,
  max: 64,
})

export const language = tinyPayload.stringEnum(
  appLanguage.Language
)

export const name = tinyPayload.string({
  min: 3,
  max: 128,
})

export const slug = tinyPayload.string({
  min: 3,
  max: 128,
})

export const taxonRank = tinyPayload.stringEnum(
  appTaxonRank.TaxonRank
)

export const title = tinyPayload.string({
  min: 3,
  max: 128,
})
