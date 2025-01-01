import * as hypertext from '@/index.ts'

export const url = import.meta.url

export const tests = {
  ['html'](): void {
    const div = undefined as unknown as HTMLDivElement
    const button = undefined as unknown as HTMLButtonElement
    hypertext.portal(button, {
      id: 'y',
      type: 'submit',
    })
    hypertext.portal(div, {
      // @ts-expect-error -- wrong element
      type: 'submit',
    })
    hypertext.portal(button, {
      type: 'submit',
      // @ts-expect-error -- unknown attribute
      unknown: '1',
    })
  },

  ['svg'](): void {
    const rect = undefined as unknown as SVGRectElement
    hypertext.portal(rect, {
      id: 'y',
      rx: 1,
    })
    hypertext.portal(rect, {
      id: 'y',
      // @ts-expect-error -- wrong element
      viewBox: [1, 2, 3, 4],
    })
    hypertext.portal(rect, {
      // @ts-expect-error -- unknown attribute
      unknown: '1',
    })
  },

  ['math'](): void {
    const mi = undefined as unknown as MathMLElement
    hypertext.portal<MathMLElement>(mi, {
      displayStyle: true,
      id: 'y',
    })
    hypertext.portal<MathMLElement>(mi, {
      id: 'y',
      // @ts-expect-error -- elements not disambiguated
      mathVariant: 'normal',
    })
    hypertext.portal(mi, {
      id: 'y',
      // @ts-expect-error -- wrong element
      stretchy: true,
    })
    hypertext.portal(mi, {
      id: 'y',
      // @ts-expect-error -- unknown attribute
      unknown: '1',
    })
  },

  ['non-element'](): void {
    hypertext.portal(
      // @ts-expect-error -- wrong type
      {},
      {
        buttonType: 'submit',
        id: 'y',
      },
    )
  },
}
