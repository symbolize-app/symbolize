import * as compute from '@/index.ts'

export const url = import.meta.url

export const tests = {
  async ['div string null'](ctx: compute.Context): Promise<void> {
    const title = compute.state<string>('a')
    await compute.txn(ctx, async () => {
      // @ts-expect-error -- can't assign null to string
      await compute.set(ctx, title, null)
    })
  },
}
