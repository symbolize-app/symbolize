import { initStyles, renderStyle } from '@tiny/ui/style'
import { message } from '@fe/ui/message'
console.log(message)
initStyles()
const s = renderStyle({
  color: 'red',
})
console.log('CLASSES', s)
