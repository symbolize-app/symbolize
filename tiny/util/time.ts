export type TimeContext = {
  performanceNow(): number
  setTimeout(callback: () => void, ms: number): unknown
}

export async function delay(
  ctx: TimeContext,
  ms: number
): Promise<void> {
  return new Promise((resolve) => {
    ctx.setTimeout(() => resolve(), ms)
  })
}

export function interval(parts: {
  hours?: number
  minutes?: number
  seconds?: number
  milliseconds?: number
}): number {
  const hours = parts.hours ?? 0
  const minutes = hours * 60 + (parts.minutes ?? 0)
  const seconds = minutes * 60 + (parts.seconds ?? 0)
  return seconds * 1000 + (parts.milliseconds ?? 0)
}

const conversionTable = {
  hours: 60 * 60 * 1000,
  minutes: 60 * 1000,
  seconds: 1000,
} as const

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
  const result = new Date()
  result.setTime(initial.getTime() + ms)
  return result
}
