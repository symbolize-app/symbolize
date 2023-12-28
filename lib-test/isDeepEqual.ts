export function isDeepEqual(a: unknown, b: unknown): boolean {
  if (a === b) {
    return true
  } else if (a === null || b === null) {
    return false
  } else if (typeof a === 'object' && typeof b === 'object') {
    if (Array.isArray(a) && Array.isArray(b)) {
      if (a.length !== b.length) {
        return false
      }
      for (let i = 0; i < a.length; i++) {
        if (!isDeepEqual(a[i], b[i])) {
          return false
        }
      }
      return true
    } else if (ArrayBuffer.isView(a) && ArrayBuffer.isView(b)) {
      const a2 = new Uint8Array(a.buffer, a.byteOffset, a.byteLength)
      const b2 = new Uint8Array(b.buffer, b.byteOffset, b.byteLength)
      if (a2.length !== b2.length) {
        return false
      }
      for (let i = 0; i < a2.length; i++) {
        if (a2[i] !== b2[i]) {
          return false
        }
      }
      return true
    } else if (a instanceof Date && b instanceof Date) {
      return a.getTime() === b.getTime()
    } else if (a.constructor !== Object || b.constructor !== Object) {
      return false
    } else {
      const a2 = new Map(Object.entries(a))
      const b2 = new Map(Object.entries(b))
      if (a2.size !== b2.size) {
        return false
      }
      for (const key of a2.keys()) {
        if (!b2.has(key)) {
          return false
        }
        if (!isDeepEqual(a2.get(key), b2.get(key))) {
          return false
        }
      }
      return true
    }
  } else {
    return false
  }
}
