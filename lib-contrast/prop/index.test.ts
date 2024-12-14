import type * as contrast from '@/index.ts'
import type * as compute from '@symbolize/lib-compute'
import type * as test from '@symbolize/lib-test'

export const all: test.TestCollection<
  compute.Context & contrast.Context
> = () => [
  import('@/prop/accent.test.ts'),
  import('@/prop/background.test.ts'),
  import('@/prop/border.test.ts'),
  import('@/prop/caret.test.ts'),
  import('@/prop/content.test.ts'),
  import('@/prop/font.test.ts'),
  import('@/prop/hyphen.test.ts'),
  import('@/prop/inset.test.ts'),
  import('@/prop/isolation.test.ts'),
  import('@/prop/line.test.ts'),
  import('@/prop/margin.test.ts'),
  import('@/prop/overflow.test.ts'),
  import('@/prop/padding.test.ts'),
  import('@/prop/pointer.test.ts'),
  import('@/prop/position.test.ts'),
  import('@/prop/punctuation.test.ts'),
  import('@/prop/size.test.ts'),
  import('@/prop/text.test.ts'),
]
