import type * as contrastDataColor from '@/data/color.ts'

export type SvgPaint =
  | contrastDataColor.Color
  | 'context-fill'
  | 'context-stroke'
