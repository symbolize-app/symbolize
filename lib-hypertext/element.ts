import type * as hypertextContext from '@/context.ts'
import * as hypertextElementAttr from '@/elementAttr.ts'
import * as hypertextFragment from '@/fragment.ts'
import * as hypertextMarker from '@/marker.ts'
import * as compute from '@symbolize/lib-compute'
import * as contrast from '@symbolize/lib-contrast'

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

export type SupportedElement = Readonly<
  HTMLElement | MathMLElement | SVGElement
>

export enum ElementFragmentMode {
  normal = 0,
  portal = 1,
}

export function elementFragment<CustomContext = unknown>(
  create: (ctx: hypertextContext.Context) => SupportedElement,
  mode: ElementFragmentMode,
  attrs: object,
): ElementFragment<CustomContext> {
  return new MutableElementFragment(create, mode, attrs)
}

class MutableElementFragment<CustomContext = unknown>
  implements
    hypertextFragment.Fragment<CustomContext>,
    hypertextContext.ScopedConvey
{
  readonly [hypertextMarker.fragmentMarker]: null = null
  private readonly mutableDeferred: (() => Promise<void> | void)[] = []
  private mutableElement: SupportedElement | null = null
  private readonly mutableSubs: compute.Computation<unknown>[] = []

  constructor(
    private readonly create: (
      ctx: hypertextContext.Context,
    ) => SupportedElement,
    private readonly mode: ElementFragmentMode,
    private readonly attrs: object,
  ) {}

  async add(
    ctx: compute.Context &
      contrast.Context &
      CustomContext &
      hypertextContext.Context,
  ): Promise<void> {
    this.mutableElement = this.create(ctx)

    const mutablePromises: Promise<void>[] = []
    let onAdd:
      | Required<
          hypertextElementAttr.Attrs<CustomContext, SupportedElement>
        >['onAdd']
      | null = null

    for (const [key, value] of Object.entries(this.attrs) as [
      keyof hypertextElementAttr.AllAttrs,
      never,
    ][]) {
      const attrDefinition = hypertextElementAttr.allAttrs[key]
      if (
        attrDefinition.kind ===
        hypertextElementAttr.ElementAttrKind.listener
      ) {
        this.addEventListener(ctx, attrDefinition.name, value)
      } else if (
        attrDefinition.kind ===
        hypertextElementAttr.ElementAttrKind.content
      ) {
        mutablePromises.push(this.appendFragment(ctx, value))
      } else if (
        attrDefinition.kind === hypertextElementAttr.ElementAttrKind.style
      ) {
        mutablePromises.push(this.bindStyle(ctx, value))
      } else if (
        attrDefinition.kind === hypertextElementAttr.ElementAttrKind.onAdd
      ) {
        onAdd = value as Required<
          hypertextElementAttr.Attrs<CustomContext, SupportedElement>
        >['onAdd']
      } else {
        mutablePromises.push(
          this.bindAttribute(
            attrDefinition.kind,
            attrDefinition.name,
            value,
          ),
        )
      }
    }

    await Promise.all(mutablePromises)

    // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition -- async
    if (onAdd && this.mutableElement) {
      await onAdd({
        ctx: {
          ...ctx,
          scopedConvey: this,
        },
        element: this.mutableElement,
      })
    }
  }

  defer(callback: () => Promise<void> | void): void {
    this.mutableDeferred.unshift(callback)
  }

  *nodes(): IterableIterator<Node> {
    if (this.mutableElement && this.mode === ElementFragmentMode.normal) {
      yield this.mutableElement
    }
  }

  async remove(): Promise<void> {
    for (const callback of this.mutableDeferred) {
      await callback()
    }
    for (const sub of this.mutableSubs) {
      compute.unsubscribe(sub)
    }

    this.mutableElement = null
  }

  subscribe(sub: compute.Computation<unknown>): void {
    this.mutableSubs.push(sub)
  }

  private addEventListener(
    ctx: compute.Context & CustomContext & hypertextContext.Context,
    type: string,
    listener: (event: Readonly<Event>) => Promise<void> | void,
  ): void {
    if (!this.mutableElement) {
      return
    }
    const elementListener = (event: Readonly<Event>): void => {
      void (async () => {
        return ctx.hypertext.scheduler.run(async () => {
          return compute.txn(ctx, async () => {
            return listener(event)
          })
        })
      })()
    }
    if (this.mode === ElementFragmentMode.portal) {
      this.defer(() => {
        this.mutableElement?.removeEventListener(type, elementListener)
      })
    }
    this.mutableElement.addEventListener(type, elementListener)
  }

  private async appendFragment(
    ctx: compute.Context &
      contrast.Context &
      CustomContext &
      hypertextContext.Context,
    fragment: hypertextFragment.FragmentOpt<CustomContext>,
  ): Promise<void> {
    const mutableFragment = hypertextFragment.toFragment(fragment)
    this.defer(async () => {
      if (
        this.mutableElement &&
        this.mode === ElementFragmentMode.portal
      ) {
        for (const node of mutableFragment.nodes()) {
          this.mutableElement.removeChild(node)
        }
      }
      await mutableFragment.remove()
    })
    await mutableFragment.add(ctx)
    if (this.mutableElement) {
      for (const node of mutableFragment.nodes()) {
        this.mutableElement.append(node)
      }
    }
  }

  private async bindAttribute(
    kind:
      | hypertextElementAttr.ElementAttrKind.boolean
      | hypertextElementAttr.ElementAttrKind.string,
    name: string,
    value: compute.NodeOpt<unknown>,
  ): Promise<void> {
    if (this.mutableElement && this.mode === ElementFragmentMode.portal) {
      const initialValue = this.mutableElement.getAttribute(name)
      this.defer(() => {
        if (initialValue === null) {
          this.mutableElement?.removeAttribute(name)
        } else {
          this.mutableElement?.setAttribute(name, initialValue)
        }
      })
    }
    this.subscribe(
      await compute.effect((value) => {
        if (!this.mutableElement) {
          return
        }
        if (
          (kind === hypertextElementAttr.ElementAttrKind.boolean &&
            value === false) ||
          value === null
        ) {
          this.mutableElement.removeAttribute(name)
        } else {
          const valueItems =
            (
              kind === hypertextElementAttr.ElementAttrKind.boolean &&
              value === true
            ) ?
              ['']
            : Array.isArray(value) ? value
            : [value]
          this.mutableElement.setAttribute(name, valueItems.join(' '))
        }
      }, value),
    )
  }

  private async bindStyle(
    ctx: compute.Context &
      contrast.Context &
      CustomContext &
      hypertextContext.Context,
    value: compute.NodeOpt<contrast.AtomOpt>,
  ): Promise<void> {
    let oldClassNames: readonly string[] = []
    if (this.mode === ElementFragmentMode.portal) {
      this.defer(() => {
        this.mutableElement?.classList.remove(...oldClassNames)
      })
    }
    this.subscribe(
      await compute.effect((value) => {
        if (!this.mutableElement) {
          return
        }
        this.mutableElement.classList.remove(...oldClassNames)
        const mutableClassNames: string[] = []
        for (const rule of contrast.compile(ctx, value)) {
          if (!ctx.hypertext.classNames.has(rule.className)) {
            ctx.hypertext.styleLayer.insertRule(
              rule.code,
              ctx.hypertext.styleLayer.cssRules.length,
            )
            ctx.hypertext.classNames.add(rule.className)
          }
          mutableClassNames.push(rule.className)
        }
        this.mutableElement.classList.add(...mutableClassNames)
        oldClassNames = mutableClassNames
      }, value),
    )
  }
}

export type ElementFragment<CustomContext = unknown> = Readonly<
  MutableElementFragment<CustomContext>
>
