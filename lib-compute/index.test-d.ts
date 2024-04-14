import * as compute from '@/index.ts'

export const url = import.meta.url

export const tests = {
  async ['string set null'](ctx: compute.Context): Promise<void> {
    const title = compute.state<string>('a')
    await compute.txn(ctx, async () => {
      // @ts-expect-error -- can't assign null to string
      await compute.set(ctx, title, null)
    })
  },

  ['invalid nesting'](): void {
    const a: compute.Node<number> = compute.pure(0)

    // @ts-expect-error -- can't assign nested type
    const b: compute.Node<number> = compute.pure(compute.pure(0))

    // @ts-expect-error -- can't assign nested type
    const c: compute.Node<number> = compute.pure(
      compute.pure(compute.pure(0)),
    )

    // @ts-expect-error -- can't assign nested type
    const d: compute.Node<number> = compute.pure(
      compute.pure(compute.pure(compute.pure(0))),
    )

    throw new Error([a, b, c, d].toString())
  },
}
