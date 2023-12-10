import type * as error from '@intertwine/lib-error'
import * as random from '@intertwine/lib-random'
import * as timeBrowser from '@intertwine/lib-time/index.browser.ts'
import * as widget from '@intertwine/lib-widget'

function main() {
  const ctx: widget.Context & error.Context = {
    ...random.initContext(),
    ...timeBrowser.initContext(),
    ...widget.initContext(window.document),
  }

  if (import.meta.env.NODE_ENV === 'development') {
    ;(async () => {
      const development = await import('@/main.development.ts')
      development.main(ctx)
    })().catch(console.error)
  }
}

main()
