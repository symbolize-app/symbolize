// Gleam copies this boundary next to the generated package modules. Four
// parent segments return to the service package root, where esbuild resolves
// these text assets with the same loaders as the original TypeScript entry.
import contentSecurityPolicy from '../../../../contentSecurityPolicy.txt'
import fontCss from '../../../../font.css'
import loaderCss from '../../../../loader.css'
import mainHtml from '../../../../main.html'
import resetCss from '../../../../reset.css'

export function content_security_policy() {
  return contentSecurityPolicy
}

export function font_css() {
  return fontCss
}

export function loader_css() {
  return loaderCss
}

export function reset_css() {
  return resetCss
}

export function main_html() {
  return mainHtml
}
