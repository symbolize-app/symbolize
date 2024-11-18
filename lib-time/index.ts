export interface Context {
  readonly time: Time
}

export interface Time {
  performanceNow(): number
  setTimeout(callback: () => void, ms: number): unknown
}

export async function delay(ctx: Context, ms: number): Promise<void> {
  return new Promise((resolve) => {
    ctx.time.setTimeout(() => {
      resolve()
    }, ms)
  })
}

class TimeImpl implements Time {
  readonly performanceNow = globalThis.performance.now.bind(
    globalThis.performance,
  )

  readonly setTimeout = globalThis.setTimeout.bind(globalThis)
}

export function time(): Time {
  return new TimeImpl()
}

const conversionTable = {
  hours: 60 * 60 * 1000,
  minutes: 60 * 1000,
  seconds: 1000,
} as const

export function interval(parts: {
  readonly hours?: number
  readonly milliseconds?: number
  readonly minutes?: number
  readonly seconds?: number
}): number {
  return (
    (parts.hours ?? 0) * conversionTable.hours +
    (parts.minutes ?? 0) * conversionTable.minutes +
    (parts.seconds ?? 0) * conversionTable.seconds +
    (parts.milliseconds ?? 0)
  )
}

export function convert(
  ms: number,
  unit: 'hours' | 'minutes' | 'seconds',
): number {
  return ms / conversionTable[unit]
}

export function subtract(
  to: Readonly<Date>,
  from: Readonly<Date>,
): number {
  return to.getTime() - from.getTime()
}

export function add(
  initial: Readonly<Date>,
  ms: Readonly<number>,
): Readonly<Date> {
  return new Date(initial.getTime() + ms)
}
