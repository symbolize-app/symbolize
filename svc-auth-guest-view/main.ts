type Empty = Record<PropertyKey, never>

function frag<T>(body: FragBody<T>): FragBuilder<T> {
  const fragScope: FragScope = {} as FragScope
  return (attr) => {
    const frag = body({ ...attr, [fragBodyScope]: fragScope })
    return {
      build(fragInstanceScopeMap) {
        const fragInstanceScope: FragInstanceScope =
          {} as FragInstanceScope
        return frag.build(
          extendFragInstanceScopeMap(
            fragInstanceScopeMap,
            fragScope,
            fragInstanceScope
          )
        )
      },
    }
  }
}

const div: FragBuilder<{ text: string }> = (attr) => {
  return {
    build(fragInstanceScopeMap) {
      const node = new HTMLDivElement()
      node.textContent = attr.text.resolve(fragInstanceScopeMap).link({
        send(value) {
          node.textContent = value
        },
      })
      return {
        node,
      }
    },
  }
}

function state<T>(
  attr: { [fragBodyScope]: FragScope },
  value: T
): Future<{ value: T }> {
  const futureInstancesMap = buildFutureInstanceMap<{ value: T }>(
    attr[fragBodyScope]
  )
  return {
    resolve(fragInstanceScopeMap) {
      return futureInstancesMap.resolve(fragInstanceScopeMap, () => {
        const futureInstanceLinks: WeakRef<
          FutureInstanceLink<{ value: T }>
        >[] = []
        let current = value
        const modify = (newValue: T): void => {
          if (newValue === current) {
            return
          }
          current = newValue
          const sendValue = {
            get value(): T {
              return newValue
            },
            set value(newValue: T) {
              modify(newValue)
            },
          }
          for (const futureInstanceLink of futureInstanceLinks) {
            futureInstanceLink.deref()?.send(sendValue)
          }
        }
        return {
          link(futureInstanceLink) {
            futureInstanceLinks.push(new WeakRef(futureInstanceLink))
            const capturedValue = current
            return {
              get value(): T {
                return capturedValue
              },
              set value(newValue: T) {
                modify(newValue)
              },
            }
          },
        }
      })
    },
  }
}

function pure<T>(value: T): Future<T> {
  return {
    resolve() {
      return {
        link(futureInstanceLink) {
          futureInstanceLink.send(value)
          return value
        },
      }
    },
  }
}

function mapFuture<T1, T2>(
  baseFuture: Future<T1>,
  transform: (base: T1) => T2
): Future<T2> {
  return {
    resolve(fragInstanceScopeMap) {
      const baseFutureInstance = baseFuture.resolve(fragInstanceScopeMap)
      const futureInstanceLinks: WeakRef<FutureInstanceLink<T2>>[] = []
      let oldBaseValue = baseFutureInstance.link({
        send(baseValue) {
          if (oldBaseValue === baseValue) {
            return
          }
          oldBaseValue = baseValue

          const value = transform(baseValue)
          if (oldValue === value) {
            return
          }
          oldValue = value

          for (const futureInstanceLink of futureInstanceLinks) {
            futureInstanceLink.deref()?.send(value)
          }
        },
      })
      let oldValue = transform(oldBaseValue)
      return {
        link(futureInstanceLink) {
          futureInstanceLinks.push(new WeakRef(futureInstanceLink))
          return oldValue
        },
      }
    },
  }
}

function buildFutureInstanceMap<T>(
  fragScope: FragScope
): FutureInstanceMap<T> {
  const futureInstances = new WeakMap<
    FragInstanceScope,
    FutureInstance<T>
  >()
  return {
    resolve(fragInstanceScopeMap, build) {
      const fragInstanceScope = fragInstanceScopeMap.resolve(fragScope)
      let futureInstance = futureInstances.get(fragInstanceScope)
      if (!futureInstance) {
        futureInstance = build()
        futureInstances.set(fragInstanceScope, futureInstance)
      }
      return futureInstance
    },
  }
}

function buildFragInstanceScopeMap(): FragInstanceScopeMap {
  return {
    resolve() {
      throw new Error('Frag scope not found')
    },
  }
}

function extendFragInstanceScopeMap(
  baseFragInstanceScopeMap: FragInstanceScopeMap,
  fragScope: FragScope,
  fragInstanceScope: FragInstanceScope
): FragInstanceScopeMap {
  return {
    resolve(otherFragScope) {
      if (otherFragScope === fragScope) {
        return fragInstanceScope
      } else {
        return baseFragInstanceScopeMap.resolve(fragScope)
      }
    },
  }
}

type Future<T> = {
  resolve(fragInstanceScopeMap: FragInstanceScopeMap): FutureInstance<T>
}

type FutureInstance<T> = {
  link(futureInstanceLink: FutureInstanceLink<T>): T
}

type FutureInstanceMap<T> = {
  resolve(
    fragInstanceScopeMap: FragInstanceScopeMap,
    build: () => FutureInstance<T>
  ): FutureInstance<T>
}

type FutureInstanceLink<T> = {
  send(value: T): void
}

const fragBodyScope = Symbol('fragBodyScope')
type FragBody<T> = (
  attr: { [K in keyof T]: Future<T[K]> } & { [fragBodyScope]: FragScope }
) => Frag

type FragBuilder<T> = (attr: { [K in keyof T]: Future<T[K]> }) => Frag

type Frag = {
  build(fragInstanceScopeMap: FragInstanceScopeMap): FragInstance
}

type FragInstance = {
  node: Node
}

const fragScopePhantom = Symbol('fragScopePhantom')
type FragScope = { [fragScopePhantom]: unknown }

const fragInstanceScopePhantom = Symbol('fragInstanceScopePhantom')
type FragInstanceScope = { [fragInstanceScopePhantom]: unknown }

type FragInstanceScopeMap = {
  resolve(fragScope: FragScope): FragInstanceScope
}

const parent = frag<{ a: { b: string } }>((attr) =>
  div({ text: mapFuture(attr.a, (a) => a.b) })
)

export function main(): void {}
