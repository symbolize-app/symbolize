import type * as conveyFragment from '@/fragment.ts'
import * as conveyMarker from '@/marker.ts'

export function empty(): conveyFragment.Fragment {
  return Empty.build()
}

class Empty implements conveyFragment.Fragment {
  private static mutableEmpty: Empty | null = null;
  readonly [conveyMarker.fragmentMarker]: null = null

  private constructor() {}

  static build(): Empty {
    if (!Empty.mutableEmpty) {
      Empty.mutableEmpty = new Empty()
    }
    return Empty.mutableEmpty
  }

  async *add(): AsyncIterableIterator<Node> {
    // Empty
  }

  async remove(): Promise<void> {
    // Empty
  }
}
