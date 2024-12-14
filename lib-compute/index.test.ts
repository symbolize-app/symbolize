/* eslint-disable @typescript-eslint/no-empty-function */
import * as compute from '@/index.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['handler, multiple'](ctx: compute.Context): Promise<void> {
    const x = compute.state('x')
    const y = compute.state('y')
    const callback = compute.handler(
      (event: number, valueX, valueY) => `${event},${valueX},${valueY}`,
      x,
      y,
    )
    test.assertEquals(await callback(1), '1,x,y')
    test.assertEquals(await callback(2), '2,x,y')
    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, 'a')
      await compute.set(ctx, y, 'b')
    })
    test.assertEquals(await callback(3), '3,a,b')
  },

  async ['pure, value'](): Promise<void> {
    const x = compute.pure('x')
    test.assertEquals(await compute.value(x), 'x')
  },

  async ['map, value'](): Promise<void> {
    const parent = compute.pure(0)
    const [callback, callbackHistory] = test.repeatMockWithHistory(
      1,
      (parentValue: number) => parentValue + 1,
    )
    const child = compute.map(callback, parent)
    test.assertDeepEquals(callbackHistory, [])
    test.assertEquals(await compute.value(parent), 0)
    test.assertEquals(await compute.value(child), 1)
    test.assertDeepEquals(callbackHistory, [[0]])
    test.assertEquals(await compute.value(child), 1)
    test.assertDeepEquals(callbackHistory, [[0]])
  },

  async ['map, multiple'](): Promise<void> {
    const x = compute.pure(2)
    const y = compute.pure(3)
    const [callback, callbackHistory] = test.repeatMockWithHistory(
      1,
      (parentX: number, parentY: number) => parentX * parentY,
    )
    const child = compute.map(callback, x, y)
    test.assertDeepEquals(callbackHistory, [])
    test.assertEquals(await compute.value(x), 2)
    test.assertEquals(await compute.value(y), 3)
    test.assertEquals(await compute.value(child), 6)
    test.assertDeepEquals(callbackHistory, [[2, 3]])
    test.assertEquals(await compute.value(child), 6)
    test.assertDeepEquals(callbackHistory, [[2, 3]])
  },

  async ['map, shortcut object'](): Promise<void> {
    const parent = compute.pure({ x: 0 })
    const child = parent.x
    test.assertDeepEquals(await compute.value(parent), { x: 0 })
    test.assertEquals(await compute.value(child), 0)
  },

  async ['map, shortcut tuple'](): Promise<void> {
    const parent = compute.pure([0] as [number])
    const child = parent[0]
    test.assertDeepEquals(await compute.value(parent), [0])
    test.assertEquals(await compute.value(child), 0)
  },

  async ['state, value'](): Promise<void> {
    const x = compute.state('x')
    test.assertEquals(await compute.value(x), 'x')
  },

  async ['state, set'](ctx: compute.Context): Promise<void> {
    const x = compute.state('x')
    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, 'a')
    })
    test.assertEquals(await compute.value(x), 'a')
  },

  async ['state, nested set'](ctx: compute.Context): Promise<void> {
    const x = compute.state('x')
    const y = compute.state('y')
    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, 'a')
      await compute.txn(ctx, async () => {
        await compute.set(ctx, y, 'b')
      })
    })
    test.assertEquals(await compute.value(x), 'a')
    test.assertEquals(await compute.value(y), 'b')
  },

  async ['state, rollback'](ctx: compute.Context): Promise<void> {
    const x = compute.state('x')
    const error = await test.assertThrowsAsync(async () =>
      compute.txn(ctx, async () => {
        await compute.set(ctx, x, 'y')
        throw new Error('rollback')
      }),
    )
    test.assertInstanceOf(error, Error)
    test.assertEquals(error.message, 'rollback')
    test.assertEquals(await compute.value(x), 'x')
  },

  async ['state, nested rollback'](ctx: compute.Context): Promise<void> {
    const x = compute.state('x')
    const y = compute.state('y')
    const error = await compute.txn(ctx, async () => {
      await compute.set(ctx, x, 'a')
      return test.assertThrowsAsync(async () =>
        compute.txn(ctx, async () => {
          await compute.set(ctx, y, 'b')
          throw new Error('rollback')
        }),
      )
    })
    test.assertInstanceOf(error, Error)
    test.assertEquals(error.message, 'rollback')
    test.assertEquals(await compute.value(x), 'a')
    test.assertEquals(await compute.value(y), 'y')
  },

  async ['derived, set simple'](ctx: compute.Context): Promise<void> {
    const parent = compute.state({ x: 'y' })
    const [callback, callbackHistory] = test.repeatMockWithHistory(
      1,
      (parentValue: { readonly x: string }) => parentValue.x,
    )
    const child = compute.derived(
      callback,
      async (newValue, parentValue) => {
        await compute.set(ctx, parent, { ...parentValue, x: newValue })
      },
      parent,
    )
    test.assertDeepEquals(callbackHistory, [])
    await compute.txn(ctx, async () => {
      await compute.set(ctx, child, 'z')
    })
    test.assertDeepEquals(callbackHistory, [])
    test.assertDeepEquals(await compute.value(parent), { x: 'z' })
    test.assertEquals(await compute.value(child), 'z')
    test.assertDeepEquals(callbackHistory, [[{ x: 'z' }]])
    test.assertEquals(await compute.value(child), 'z')
    test.assertDeepEquals(callbackHistory, [[{ x: 'z' }]])
  },

  async ['derived, set multiple'](ctx: compute.Context): Promise<void> {
    const x = compute.state({ x: 'x' })
    const y = compute.state({ y: 'y' })
    const [callback, callbackHistory] = test.repeatMockWithHistory(
      1,
      (
        parentX: { readonly x: string },
        parentY: { readonly y: string },
      ) => ({ ...parentX, ...parentY }),
    )
    const child = compute.derived(
      callback,
      async (newValue, parentX, parentY) => {
        await compute.set(ctx, x, { ...parentX, x: newValue.x })
        await compute.set(ctx, y, { ...parentY, y: newValue.y })
      },
      x,
      y,
    )
    test.assertDeepEquals(callbackHistory, [])
    await compute.txn(ctx, async () => {
      await compute.set(ctx, child, { x: 'a', y: 'b' })
    })
    test.assertDeepEquals(callbackHistory, [])
    test.assertDeepEquals(await compute.value(x), { x: 'a' })
    test.assertDeepEquals(await compute.value(y), { y: 'b' })
    test.assertDeepEquals(await compute.value(child), { x: 'a', y: 'b' })
    test.assertDeepEquals(callbackHistory, [[{ x: 'a' }, { y: 'b' }]])
    test.assertDeepEquals(await compute.value(child), { x: 'a', y: 'b' })
    test.assertDeepEquals(callbackHistory, [[{ x: 'a' }, { y: 'b' }]])
  },

  async ['derived, set shortcut object'](
    ctx: compute.Context,
  ): Promise<void> {
    const parent = compute.state({ x: 'y' })
    const child = parent.x
    await compute.txn(ctx, async () => {
      await compute.set(ctx, child, 'z')
    })
    test.assertDeepEquals(await compute.value(parent), { x: 'z' })
    test.assertEquals(await compute.value(child), 'z')
  },

  async ['derived, set shortcut tuple'](
    ctx: compute.Context,
  ): Promise<void> {
    const parent = compute.state(['y'] as [string])
    const child = parent[0]
    await compute.txn(ctx, async () => {
      await compute.set(ctx, child, 'z')
    })
    test.assertDeepEquals(await compute.value(parent), ['z'])
    test.assertEquals(await compute.value(child), 'z')
  },

  async ['effect, pure'](): Promise<void> {
    const x = compute.pure('x')
    const [callback, callbackHistory] = test.repeatMockWithHistory(
      1,
      (_value: string) => {},
    )
    await compute.effect(callback, x)
    test.assertDeepEquals(callbackHistory, [['x']])
    test.assertEquals(await compute.value(x), 'x')
  },

  async ['effect, state change simple'](
    ctx: compute.Context,
  ): Promise<void> {
    const x = compute.state('x')
    const [callback, callbackHistory] = test.repeatMockWithHistory(
      2,
      (_value: string) => {},
    )
    await compute.effect(callback, x)
    test.assertDeepEquals(callbackHistory, [['x']])
    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, 'a')
    })
    test.assertDeepEquals(callbackHistory, [['x'], ['a']])
    test.assertEquals(await compute.value(x), 'a')
  },

  async ['effect, state change multiple'](
    ctx: compute.Context,
  ): Promise<void> {
    const x = compute.state('x')
    const y = compute.state('y')
    const [callback, callbackHistory] = test.repeatMockWithHistory(
      2,
      (_valueX: string, _valueY: string) => {},
    )
    await compute.effect(callback, x, y)
    test.assertDeepEquals(callbackHistory, [['x', 'y']])
    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, 'a')
      await compute.set(ctx, y, 'b')
    })
    test.assertDeepEquals(callbackHistory, [
      ['x', 'y'],
      ['a', 'b'],
    ])
    test.assertEquals(await compute.value(x), 'a')
    test.assertEquals(await compute.value(y), 'b')
  },

  async ['effect, partial graph update'](
    ctx: compute.Context,
  ): Promise<void> {
    const x = compute.state('x')
    const [x2Callback, x2CallbackHistory] = test.repeatMockWithHistory(
      2,
      (valueY: string) => `${valueY}/${valueY}`,
    )
    const x2 = compute.map(x2Callback, x)

    const y = compute.state('y')
    const [y2Callback, y2CallbackHistory] = test.repeatMockWithHistory(
      1,
      (valueY: string) => `${valueY}?${valueY}`,
    )
    const y2 = compute.map(y2Callback, y)

    const [effectCallback, effectCallbackHistory] =
      test.repeatMockWithHistory(
        2,
        (_valueX: string, _valueY: string) => {},
      )
    await compute.effect(effectCallback, x2, y2)
    test.assertDeepEquals(x2CallbackHistory, [['x']])
    test.assertDeepEquals(y2CallbackHistory, [['y']])
    test.assertDeepEquals(effectCallbackHistory, [['x/x', 'y?y']])

    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, 'a')
    })
    test.assertDeepEquals(x2CallbackHistory, [['x'], ['a']])
    test.assertDeepEquals(y2CallbackHistory, [['y']])
    test.assertDeepEquals(effectCallbackHistory, [
      ['x/x', 'y?y'],
      ['a/a', 'y?y'],
    ])
    test.assertEquals(await compute.value(x), 'a')
    test.assertEquals(await compute.value(y), 'y')
  },

  async ['effect, unsubscribe'](ctx: compute.Context): Promise<void> {
    const x = compute.state('x')
    const [x2Callback, x2CallbackHistory] = test.repeatMockWithHistory(
      3,
      (value: string) => `${value}2`,
    )
    const x2 = compute.map(x2Callback, x)
    const [x3Callback, x3CallbackHistory] = test.repeatMockWithHistory(
      2,
      (value: string) => `${value}3`,
    )
    const x3 = compute.map(x3Callback, x)
    const x4Callback = test.repeatMock(0, (value: string) => `${value}4`)
    compute.map(x4Callback, x)

    const [effect1Callback, effect1CallbackHistory] =
      test.repeatMockWithHistory(3, (_valueX2: string) => {})
    await compute.effect(effect1Callback, x2)
    test.assertDeepEquals(x2CallbackHistory, [['x']])
    test.assertDeepEquals(x3CallbackHistory, [])
    test.assertDeepEquals(effect1CallbackHistory, [['x2']])

    const [effect2Callback, effect2CallbackHistory] =
      test.repeatMockWithHistory(
        2,
        (_valueX2: string, _valueX3: string) => {},
      )
    const effect2 = await compute.effect(effect2Callback, x2, x3)
    test.assertDeepEquals(x2CallbackHistory, [['x']])
    test.assertDeepEquals(x3CallbackHistory, [['x']])
    test.assertDeepEquals(effect2CallbackHistory, [['x2', 'x3']])

    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, 'a')
    })
    test.assertDeepEquals(x2CallbackHistory, [['x'], ['a']])
    test.assertDeepEquals(x3CallbackHistory, [['x'], ['a']])
    test.assertDeepEquals(effect1CallbackHistory, [['x2'], ['a2']])
    test.assertDeepEquals(effect2CallbackHistory, [
      ['x2', 'x3'],
      ['a2', 'a3'],
    ])
    test.assertEquals(await compute.value(x), 'a')

    compute.unsubscribe(effect2)
    await compute.txn(ctx, async () => {
      await compute.set(ctx, x, 'b')
    })
    test.assertDeepEquals(x2CallbackHistory, [['x'], ['a'], ['b']])
    test.assertDeepEquals(x3CallbackHistory, [['x'], ['a']])
    test.assertDeepEquals(effect1CallbackHistory, [['x2'], ['a2'], ['b2']])
    test.assertDeepEquals(effect2CallbackHistory, [
      ['x2', 'x3'],
      ['a2', 'a3'],
    ])
    test.assertEquals(await compute.value(x), 'b')
  },
}
