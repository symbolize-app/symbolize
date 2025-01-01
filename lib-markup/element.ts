import type * as markupContext from '@/context.ts'
import * as markupElementAttr from '@/elementAttr.ts'
import * as markupFragment from '@/fragment.ts'
import * as markupMarker from '@/marker.ts'
import * as dataflow from '@symbolize/lib-dataflow'
import * as styling from '@symbolize/lib-styling'

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
  create: (ctx: markupContext.Context) => SupportedElement,
  mode: ElementFragmentMode,
  attrs: object,
): ElementFragment<CustomContext> {
  return new MutableElementFragment(create, mode, attrs)
}

class MutableElementFragment<CustomContext = unknown>
  implements
    markupFragment.Fragment<CustomContext>,
    markupContext.ScopedConvey
{
  readonly [markupMarker.fragmentMarker]: null = null
  private readonly mutableDeferred: (() => Promise<void> | void)[] = []
  private mutableElement: SupportedElement | null = null
  private readonly mutableSubs: dataflow.Computation<unknown>[] = []

  constructor(
    private readonly create: (
      ctx: markupContext.Context,
    ) => SupportedElement,
    private readonly mode: ElementFragmentMode,
    private readonly attrs: object,
  ) {}

  async add(
    ctx: CustomContext &
      dataflow.Context &
      markupContext.Context &
      styling.Context,
  ): Promise<void> {
    this.mutableElement = this.create(ctx)

    const mutablePromises: Promise<void>[] = []
    let onAdd:
      | Required<
          markupElementAttr.Attrs<CustomContext, SupportedElement>
        >['onAdd']
      | null = null

    for (const [key, value] of Object.entries(this.attrs) as [
      keyof markupElementAttr.AllAttrs,
      never,
    ][]) {
      const attrDefinition = markupElementAttr.allAttrs[key]
      if (
        attrDefinition.kind === markupElementAttr.ElementAttrKind.listener
      ) {
        this.addEventListener(ctx, attrDefinition.name, value)
      } else if (
        attrDefinition.kind === markupElementAttr.ElementAttrKind.content
      ) {
        mutablePromises.push(this.appendFragment(ctx, value))
      } else if (
        attrDefinition.kind === markupElementAttr.ElementAttrKind.style
      ) {
        mutablePromises.push(this.bindStyle(ctx, value))
      } else if (
        attrDefinition.kind === markupElementAttr.ElementAttrKind.onAdd
      ) {
        onAdd = value as Required<
          markupElementAttr.Attrs<CustomContext, SupportedElement>
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
      dataflow.unsubscribe(sub)
    }

    this.mutableElement = null
  }

  subscribe(sub: dataflow.Computation<unknown>): void {
    this.mutableSubs.push(sub)
  }

  private addEventListener(
    ctx: CustomContext & dataflow.Context & markupContext.Context,
    type: string,
    listener: (event: Readonly<Event>) => Promise<void> | void,
  ): void {
    if (!this.mutableElement) {
      return
    }
    const elementListener = (event: Readonly<Event>): void => {
      void (async () => {
        return ctx.markup.scheduler.run(async () => {
          return dataflow.txn(ctx, async () => {
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
    ctx: CustomContext &
      dataflow.Context &
      markupContext.Context &
      styling.Context,
    fragment: markupFragment.FragmentOpt<CustomContext>,
  ): Promise<void> {
    const mutableFragment = markupFragment.toFragment(fragment)
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
      | markupElementAttr.ElementAttrKind.boolean
      | markupElementAttr.ElementAttrKind.string,
    name: string,
    value: dataflow.NodeOpt<unknown>,
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
      await dataflow.effect((value) => {
        if (!this.mutableElement) {
          return
        }
        if (
          (kind === markupElementAttr.ElementAttrKind.boolean &&
            value === false) ||
          value === null
        ) {
          this.mutableElement.removeAttribute(name)
        } else {
          const valueItems =
            (
              kind === markupElementAttr.ElementAttrKind.boolean &&
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
    ctx: CustomContext &
      dataflow.Context &
      markupContext.Context &
      styling.Context,
    value: dataflow.NodeOpt<styling.AtomOpt>,
  ): Promise<void> {
    let oldClassNames: readonly string[] = []
    if (this.mode === ElementFragmentMode.portal) {
      this.defer(() => {
        this.mutableElement?.classList.remove(...oldClassNames)
      })
    }
    this.subscribe(
      await dataflow.effect((value) => {
        if (!this.mutableElement) {
          return
        }
        this.mutableElement.classList.remove(...oldClassNames)
        const mutableClassNames: string[] = []
        for (const rule of styling.compile(ctx, value)) {
          if (!ctx.markup.classNames.has(rule.className)) {
            ctx.markup.styleLayer.insertRule(
              rule.code,
              ctx.markup.styleLayer.cssRules.length,
            )
            ctx.markup.classNames.add(rule.className)
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
