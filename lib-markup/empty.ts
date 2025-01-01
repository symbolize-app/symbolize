import type * as markupFragment from '@/fragment.ts'
import * as markupMarker from '@/marker.ts'

export function empty(): markupFragment.Fragment {
  return Empty.build()
}

class Empty implements markupFragment.Fragment {
  private static mutableEmpty: Empty | null = null;
  readonly [markupMarker.fragmentMarker]: null = null

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
