import type * as conveyContext from '@/context.ts'
import * as conveyElement from '@/element.ts'
import * as conveyFragment from '@/fragment.ts'
import type * as conveyHtmlAttrs from '@/htmlAttrs.ts'
import * as conveyMarker from '@/marker.ts'
import * as compute from '@intertwine/lib-compute'

export const html = new Proxy(
  {},
  {
    get(_mutableTarget: object, tag: string, _receiver: unknown): unknown {
      return (attrs: object) => new HtmlElementFragment(tag, attrs)
    },
  },
) as unknown as {
  readonly [Key in keyof conveyHtmlAttrs.AttrsTagNameMap<unknown> &
    keyof HTMLElementTagNameMap]: <CustomContext = unknown>(
    attrs: conveyHtmlAttrs.AttrsTagNameMap<CustomContext>[Key],
  ) => conveyFragment.Fragment<CustomContext>
}

class HtmlElementFragment<CustomContext = unknown>
  implements conveyFragment.Fragment<CustomContext>
{
  readonly [conveyMarker.fragmentMarker]: null = null
  private mutableFragment: conveyFragment.Fragment<CustomContext> | null =
    null
  private readonly mutableSubs: compute.Computation<unknown>[] = []

  constructor(
    private readonly tag: string,
    private readonly attrs: object,
  ) {}

  async *add(
    ctx: compute.Context & conveyContext.Context & CustomContext,
  ): AsyncIterableIterator<Node> {
    const mutableElement = ctx.convey.document.createElement(this.tag)

    const mutablePromises: Promise<void>[] = []

    for (const [key, value] of Object.entries(this.attrs) as [
      never,
      never,
    ][]) {
      const eventType =
        conveyElement.elementEventListenerPattern.exec(key)?.groups?.[
          'type'
        ]
      if (eventType) {
        conveyElement.addElementEventListener(
          ctx,
          mutableElement,
          eventType.toLowerCase(),
          value,
        )
      } else if (key === 'content') {
        this.mutableFragment = conveyFragment.toFragment(value)
        mutablePromises.push(
          conveyElement.appendFragmentToElement(
            ctx,
            mutableElement,
            this.mutableFragment,
          ),
        )
      } else {
        mutablePromises.push(
          (async () => {
            this.mutableSubs.push(
              await compute.effect((value) => {
                mutableElement[key] = value
              }, value),
            )
          })(),
        )
      }
    }

    await Promise.all(mutablePromises)

    yield mutableElement
  }

  async remove(): Promise<void> {
    if (this.mutableFragment) {
      await this.mutableFragment.remove()
    }
    for (const sub of this.mutableSubs) {
      compute.unsubscribe(sub)
    }
  }
}
