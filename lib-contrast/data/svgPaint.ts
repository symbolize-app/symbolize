import type * as contrastDataColor from '@/data/color.ts'

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/SVG/Content_type#paint
 */
export type SvgPaint =
  | contrastDataColor.Color
  | 'context-fill'
  | 'context-stroke'
  | 'none'
