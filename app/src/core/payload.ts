import * as payload from '@tiny/core/payload.ts'

export const checkId = payload.checkString({
  min: 64,
  max: 64,
})

export const checkEmail = payload.checkString({
  min: 5,
  max: 64,
})

export const checkHandle = payload.checkString({
  min: 3,
  max: 64,
})

export const checkTitle = payload.checkString({
  min: 3,
  max: 128,
})

export const checkSlug = payload.checkString({
  min: 3,
  max: 128,
})

export const checkContent = payload.checkString({
  min: 3,
  max: 4096,
})
