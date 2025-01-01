import type * as hypertextFragment from '@/fragment.ts'
import * as hypertextMarker from '@/marker.ts'

export function empty(): hypertextFragment.Fragment {
  return Empty.build()
}

class Empty implements hypertextFragment.Fragment {
  private static mutableEmpty: Empty | null = null;
  readonly [hypertextMarker.fragmentMarker]: null = null

  private constructor() {}

  static build(): Empty {
    if (!Empty.mutableEmpty) {
      Empty.mutableEmpty = new Empty()
    }
    return Empty.mutableEmpty
  }

  async add(): Promise<void> {
    // Empty
  }

  *nodes(): IterableIterator<Node> {
    // Empty
  }

  async remove(): Promise<void> {
    // Empty
  }
}
