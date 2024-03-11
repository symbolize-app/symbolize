import type * as conveyContext from '@/context.ts'
import type * as conveyFragment from '@/fragment.ts'
import * as compute from '@intertwine/lib-compute'

export function replaceBetween(
  startNode: Readonly<Node>,
  endNode: Readonly<Node>,
  innerNodes: readonly Node[],
): readonly Node[] | null {
  if (startNode.parentElement !== endNode.parentElement) {
    throw new Error("Can't replace with different parents")
  }

  const parentElement = startNode.parentElement
  if (!parentElement) {
    return innerNodes
  }

  const mutableNewNodes: Node[] = []
  let inside = false
  for (const oldNode of parentElement.childNodes) {
    if (oldNode === startNode) {
      mutableNewNodes.push(startNode)
      mutableNewNodes.push(...innerNodes)
      inside = true
    } else if (oldNode === endNode) {
      if (!inside) {
        throw new Error('End before start')
      }
      mutableNewNodes.push(endNode)
      inside = false
    } else if (!inside) {
      mutableNewNodes.push(oldNode)
    }
  }

  parentElement.replaceChildren(...mutableNewNodes)
  return null
}

export async function appendFragmentToElement<CustomContext>(
  ctx: compute.Context & conveyContext.Context & CustomContext,
  mutableElement: Element,
  fragment: conveyFragment.Fragment<CustomContext>,
): Promise<void> {
  for await (const node of fragment.add(ctx)) {
    mutableElement.append(node)
  }
}

export const elementEventListenerPattern: Readonly<RegExp> =
  /^on(?<type>[A-Z].+)/

export function addElementEventListener(
  ctx: compute.Context & conveyContext.Context,
  mutableElement: Element,
  type: string,
  listener: (event: Readonly<Event>) => Promise<void> | void,
): void {
  mutableElement.addEventListener(type, (event) => {
    void (async () => {
      return ctx.convey.scheduler.run(async () => {
        return compute.txn(ctx, async () => {
          return listener(event)
        })
      })
    })()
  })
}
