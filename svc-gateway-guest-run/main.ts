import type * as tinyError from '@intertwine/lib-error/error.ts'
import * as tinyRandom from '@intertwine/lib-random'
import * as tinyTimeBrowser from '@intertwine/lib-time/time.browser.ts'
import * as tinyWidget from '@intertwine/lib-widget/widget.ts'

function main() {
  const ctx: tinyWidget.Context & tinyError.Context = {
    ...tinyRandom.initContext(),
    ...tinyTimeBrowser.initContext(),
    ...tinyWidget.initContext(window.document),
  }

  if (import.meta.env.NODE_ENV === 'development') {
    ;(async () => {
      const development = await import(
        '@/main.development.ts'
      )
      development.main(ctx)
    })().catch(console.error)
  }
}

main()
