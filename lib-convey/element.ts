import type * as conveyContext from '@/context.ts'
import * as conveyElementAttrs from '@/elementAttrs.ts'
import * as conveyFragment from '@/fragment.ts'
import * as conveyMarker from '@/marker.ts'
import * as compute from '@intertwine/lib-compute'
import * as contrast from '@intertwine/lib-contrast'

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

export class ElementFragment<CustomContext = unknown>
  implements
    conveyFragment.Fragment<CustomContext>,
    conveyContext.ScopedConvey
{
  readonly [conveyMarker.fragmentMarker]: null = null
  private readonly mutableDeferred: (() => Promise<void> | void)[] = []
  private mutableFragment: conveyFragment.Fragment<CustomContext> | null =
    null
  private readonly mutableSubs: compute.Computation<unknown>[] = []

  constructor(
    private readonly namespace: string | null,
    private readonly tag: string,
    private readonly attrs: object,
  ) {}

  async *add(
    ctx: compute.Context &
      contrast.Context &
      conveyContext.Context &
      CustomContext,
  ): AsyncIterableIterator<Node> {
    const mutableElement =
      this.namespace ?
        ctx.convey.document.createElementNS(this.namespace, this.tag)
      : ctx.convey.document.createElement(this.tag)

    const mutablePromises: Promise<void>[] = []
    let onAdd:
      | Required<conveyElementAttrs.Attrs<CustomContext, Element>>['onAdd']
      | null = null

    for (const [key, value] of Object.entries(this.attrs) as [
      keyof conveyElementAttrs.AllAttrs,
      never,
    ][]) {
      const attrDefinition = conveyElementAttrs.allAttrs[key]
      if (
        attrDefinition.kind === conveyElementAttrs.ElementAttrKind.listener
      ) {
        this.addEventListener(
          ctx,
          mutableElement,
          attrDefinition.name,
          value,
        )
      } else if (
        attrDefinition.kind === conveyElementAttrs.ElementAttrKind.content
      ) {
        mutablePromises.push(
          this.appendFragment(ctx, mutableElement, value),
        )
      } else if (
        attrDefinition.kind === conveyElementAttrs.ElementAttrKind.style
      ) {
        this.bindStyle(ctx, mutableElement, value)
      } else if (
        attrDefinition.kind === conveyElementAttrs.ElementAttrKind.onAdd
      ) {
        onAdd = value as Required<
          conveyElementAttrs.Attrs<CustomContext, Element>
        >['onAdd']
      } else {
        mutablePromises.push(
          this.bindAttribute(
            mutableElement,
            attrDefinition.kind,
            attrDefinition.name,
            value,
          ),
        )
      }
    }

    await Promise.all(mutablePromises)

    if (onAdd) {
      await onAdd({
        ctx: {
          ...ctx,
          scopedConvey: this,
        },
        element: mutableElement,
      })
    }

    yield mutableElement
  }

  defer(callback: () => Promise<void> | void): void {
    this.mutableDeferred.unshift(callback)
  }

  async remove(): Promise<void> {
    if (this.mutableFragment) {
      await this.mutableFragment.remove()
    }
    for (const callback of this.mutableDeferred) {
      await callback()
    }
    for (const sub of this.mutableSubs) {
      compute.unsubscribe(sub)
    }
  }

  subscribe(sub: compute.Computation<unknown>): void {
    this.mutableSubs.push(sub)
  }

  private addEventListener(
    ctx: compute.Context & conveyContext.Context & CustomContext,
    mutableElement: Element,
    type: string,
    listener: (event: Readonly<Event>) => Promise<void> | void,
  ): void {
    mutableElement.addEventListener(type, (event) => {
      void (async () => {
        return ctx.convey.mutableScheduler.run(async () => {
          return compute.txn(ctx, async () => {
            return listener(event)
          })
        })
      })()
    })
  }

  private async appendFragment(
    ctx: compute.Context &
      contrast.Context &
      conveyContext.Context &
      CustomContext,
    mutableElement: Element,
    fragment: conveyFragment.FragmentOpt<CustomContext>,
  ): Promise<void> {
    this.mutableFragment = conveyFragment.toFragment(fragment)
    for await (const node of this.mutableFragment.add(ctx)) {
      mutableElement.append(node)
    }
  }

  private async bindAttribute(
    mutableElement: Element,
    kind:
      | conveyElementAttrs.ElementAttrKind.boolean
      | conveyElementAttrs.ElementAttrKind.string,
    name: string,
    value: compute.NodeOpt<unknown>,
  ): Promise<void> {
    this.subscribe(
      await compute.effect((value) => {
        if (
          (kind === conveyElementAttrs.ElementAttrKind.boolean &&
            value === false) ||
          value === null
        ) {
          mutableElement.removeAttribute(name)
        } else {
          const valueItems =
            (
              kind === conveyElementAttrs.ElementAttrKind.boolean &&
              value === true
            ) ?
              ['']
            : Array.isArray(value) ? value
            : [value]
          mutableElement.setAttribute(name, valueItems.join(' '))
        }
      }, value),
    )
  }

  private bindStyle(
    ctx: compute.Context &
      contrast.Context &
      conveyContext.Context &
      CustomContext,
    mutableElement: Element,
    value: contrast.Style,
  ): void {
    for (const rule of contrast.compile(ctx, value)) {
      if (!ctx.convey.classNames.has(rule.className)) {
        ctx.convey.styleLayer.insertRule(
          rule.code,
          ctx.convey.styleLayer.cssRules.length,
        )
        ctx.convey.classNames.add(rule.className)
      }
      mutableElement.classList.add(rule.className)
    }
  }
}
