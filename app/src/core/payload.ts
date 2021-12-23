import * as appDocumentType from '@fe/core/documentType.ts'
import * as appLanguage from '@fe/core/language.ts'
import * as appTaxonRank from '@fe/core/taxonRank.ts'
import * as payload from '@tiny/core/payload.ts'

export const content = payload.string({
  min: 1,
  max: 4096,
})

export const documentType = payload.stringEnum(
  appDocumentType.DocumentType
)

export const email = payload.string({
  min: 5,
  max: 64,
})

export const handle = payload.string({
  min: 3,
  max: 64,
})

export const id = payload.string({
  min: 64,
  max: 64,
})

export const language = payload.stringEnum(
  appLanguage.Language
)

export const name = payload.string({
  min: 3,
  max: 128,
})

export const slug = payload.string({
  min: 3,
  max: 128,
})

export const taxonRank = payload.stringEnum(
  appTaxonRank.TaxonRank
)

export const title = payload.string({
  min: 3,
  max: 128,
})
