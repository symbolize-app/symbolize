import * as appLanguage from '@fe/core/language.ts'
import * as payload from '@tiny/core/payload.ts'

export const checkContent = payload.checkString({
  min: 3,
  max: 4096,
})

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

export const checkSlug = payload.checkString({
  min: 3,
  max: 128,
})

export const checkTitle = payload.checkString({
  min: 3,
  max: 128,
})
