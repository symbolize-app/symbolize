import * as widget from '@tiny/ui/widget.ts'

export function collectOne<
  Result extends string | Node = string | Node
>(item: widget.Widget): Result {
  const results = widget.collect([item])
  if (!results.length || results.length > 1) {
    throw new Error('Expected one result')
  }
  return results[0] as Result
}
