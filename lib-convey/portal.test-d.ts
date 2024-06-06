import * as convey from '@/index.ts'

export const url = import.meta.url

export const tests = {
  ['html'](): void {
    const div = undefined as unknown as HTMLDivElement
    const button = undefined as unknown as HTMLButtonElement
    convey.portal(button, {
      id: 'y',
      type: 'submit',
    })
    convey.portal(div, {
      // @ts-expect-error -- wrong element
      type: 'submit',
    })
    convey.portal(button, {
      type: 'submit',
      // @ts-expect-error -- unknown attribute
      unknown: '1',
    })
  },

  ['svg'](): void {
    const rect = undefined as unknown as SVGRectElement
    convey.portal(rect, {
      id: 'y',
      rx: 1,
    })
    convey.portal(rect, {
      id: 'y',
      // @ts-expect-error -- wrong element
      viewBox: [1, 2, 3, 4],
    })
    convey.portal(rect, {
      // @ts-expect-error -- unknown attribute
      unknown: '1',
    })
  },

  ['math'](): void {
    const mi = undefined as unknown as MathMLElement
    convey.portal<MathMLElement>(mi, {
      displayStyle: true,
      id: 'y',
    })
    convey.portal<MathMLElement>(mi, {
      id: 'y',
      // @ts-expect-error -- elements not disambiguated
      mathVariant: 'normal',
    })
    convey.portal(mi, {
      id: 'y',
      // @ts-expect-error -- wrong element
      stretchy: true,
    })
    convey.portal(mi, {
      id: 'y',
      // @ts-expect-error -- unknown attribute
      unknown: '1',
    })
  },

  ['non-element'](): void {
    convey.portal(
      // @ts-expect-error -- wrong type
      {},
      {
        buttonType: 'submit',
        id: 'y',
      },
    )
  },
}
