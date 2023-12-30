export type Context = {
  time: {
    performanceNow(): number
    setTimeout(callback: () => void, ms: number): unknown
  }
}

export async function delay(ctx: Context, ms: number): Promise<void> {
  return new Promise((resolve) => {
    ctx.time.setTimeout(() => resolve(), ms)
  })
}

const conversionTable = {
  hours: 60 * 60 * 1000,
  minutes: 60 * 1000,
  seconds: 1000,
} as const

export function interval(parts: {
  hours?: number
  minutes?: number
  seconds?: number
  milliseconds?: number
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
  unit: 'hours' | 'minutes' | 'seconds'
): number {
  return ms / conversionTable[unit]
}

export function subtract(to: Date, from: Date): number {
  return to.getTime() - from.getTime()
}

export function add(initial: Date, ms: number): Date {
  return new Date(initial.getTime() + ms)
}
