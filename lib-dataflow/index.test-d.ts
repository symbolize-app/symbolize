import * as dataflow from '@/index.ts'

export const url = import.meta.url

export const tests = {
  async ['string set null'](ctx: dataflow.Context): Promise<void> {
    const title = dataflow.state<string>('a')
    await dataflow.txn(ctx, async () => {
      // @ts-expect-error -- can't assign null to string
      await dataflow.set(ctx, title, null)
    })
  },

  ['invalid nesting'](): void {
    const a: dataflow.Node<number> = dataflow.pure(0)

    // @ts-expect-error -- can't assign nested type
    const b: dataflow.Node<number> = dataflow.pure(dataflow.pure(0))

    // @ts-expect-error -- can't assign nested type
    const c: dataflow.Node<number> = dataflow.pure(
      dataflow.pure(dataflow.pure(0)),
    )

    // @ts-expect-error -- can't assign nested type
    const d: dataflow.Node<number> = dataflow.pure(
      dataflow.pure(dataflow.pure(dataflow.pure(0))),
    )

    throw new Error([a, b, c, d].toString())
  },
}
