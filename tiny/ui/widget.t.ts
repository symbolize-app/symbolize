import * as widget from '@tiny/ui/widget.ts'

export function collectOne(
  item: widget.Widget
): string | Node {
  const results = widget.collect([item])
  if (!results.length || results.length > 1) {
    throw new Error('Expected one result')
  }
  return results[0]
}
