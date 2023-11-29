import type * as tinyTest from '@intertwine/lib-test/index.ts'
import type * as tinyWidget from '@intertwine/lib-widget/widget.ts'

export const all: tinyTest.TestCollection<
  tinyWidget.Context
> = () => [
  import('@intertwine/lib-payload/index.test.ts'),
  import('@intertwine/lib-test/hex.test.ts'),
  import(
    '@intertwine/svc-auth-guest-display/widget/button.test.ts'
  ),
  import(
    '@intertwine/svc-auth-guest-display/widget/member.test.ts'
  ),
]
