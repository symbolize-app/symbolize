import * as markup from '@/index.ts'

export const url = import.meta.url

export const tests = {
  ['html'](): void {
    const div = undefined as unknown as HTMLDivElement
    const button = undefined as unknown as HTMLButtonElement
    markup.portal(button, {
      id: 'y',
      type: 'submit',
    })
    markup.portal(div, {
      // @ts-expect-error -- wrong element
      type: 'submit',
    })
    markup.portal(button, {
      type: 'submit',
      // @ts-expect-error -- unknown attribute
      unknown: '1',
    })
  },

  ['svg'](): void {
    const rect = undefined as unknown as SVGRectElement
    markup.portal(rect, {
      id: 'y',
      rx: 1,
    })
    markup.portal(rect, {
      id: 'y',
      // @ts-expect-error -- wrong element
      viewBox: [1, 2, 3, 4],
    })
    markup.portal(rect, {
      // @ts-expect-error -- unknown attribute
      unknown: '1',
    })
  },

  ['math'](): void {
    const mi = undefined as unknown as MathMLElement
    markup.portal<MathMLElement>(mi, {
      displayStyle: true,
      id: 'y',
    })
    markup.portal<MathMLElement>(mi, {
      id: 'y',
      // @ts-expect-error -- elements not disambiguated
      mathVariant: 'normal',
    })
    markup.portal(mi, {
      id: 'y',
      // @ts-expect-error -- wrong element
      stretchy: true,
    })
    markup.portal(mi, {
      id: 'y',
      // @ts-expect-error -- unknown attribute
      unknown: '1',
    })
  },

  ['non-element'](): void {
    markup.portal(
      // @ts-expect-error -- wrong type
      {},
      {
        buttonType: 'submit',
        id: 'y',
      },
    )
  },
}
