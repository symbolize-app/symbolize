export function main(): void {
  if (process.send) {
    process.send('ready')
  }
}
