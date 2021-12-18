import * as appDocumentType from '@fe/core/documentType.ts'
import * as appLanguage from '@fe/core/language.ts'
import * as appTaxonRank from '@fe/core/taxonRank.ts'
import * as payload from '@tiny/core/payload.ts'

export const checkContent = payload.checkString({
  min: 1,
  max: 4096,
})

export const checkDocumentType = payload.checkStringEnum(
  appDocumentType.DocumentType
)

export const checkEmail = payload.checkString({
  min: 5,
  max: 64,
})

export const checkHandle = payload.checkString({
  min: 3,
  max: 64,
})

export const checkId = payload.checkString({
  min: 64,
  max: 64,
})

export const checkLanguage = payload.checkStringEnum(
  appLanguage.Language
)

export const checkName = payload.checkString({
  min: 3,
  max: 128,
})

export const checkSlug = payload.checkString({
  min: 3,
  max: 128,
})

export const checkTaxonRank = payload.checkStringEnum(
  appTaxonRank.TaxonRank
)

export const checkTitle = payload.checkString({
  min: 3,
  max: 128,
})
