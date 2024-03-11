import type * as conveyContext from '@/context.ts'
import * as conveyElement from '@/element.ts'
import * as conveyFragment from '@/fragment.ts'
import * as conveyMarker from '@/marker.ts'
import type * as conveySvgAttrs from '@/svgAttrs.ts'
import * as compute from '@intertwine/lib-compute'

export const svg = new Proxy(
  {},
  {
    get(
      _mutableTarget: object,
      property: string,
      _receiver: unknown,
    ): unknown {
      return (attrs: object) => new SvgElementFragment(property, attrs)
    },
  },
) as unknown as {
  readonly [Key in keyof conveySvgAttrs.AttrsTagNameMap<unknown> &
    keyof SVGElementTagNameMap]: <CustomContext = unknown>(
    attrs: conveySvgAttrs.AttrsTagNameMap<CustomContext>[Key],
  ) => conveyFragment.Fragment<CustomContext>
}

class SvgElementFragment<CustomContext = unknown>
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
    const mutableElement = ctx.convey.document.createElementNS(
      'http://www.w3.org/2000/svg',
      this.tag,
    )

    const mutablePromises: Promise<void>[] = []

    for (const [key, value] of Object.entries(this.attrs) as [
      never,
      never,
    ][]) {
      const eventType =
        conveyElement.elementEventListenerPattern.exec(key)?.groups?.[
          'test'
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
                if ((value as unknown) === null) {
                  mutableElement.removeAttribute(key)
                } else {
                  const valueItems = Array.isArray(value) ? value : [value]
                  mutableElement.setAttribute(key, valueItems.join(' '))
                }
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
