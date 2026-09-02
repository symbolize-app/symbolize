import gleam/io
import lib_concurrency as concurrency

pub fn main() {
  let ordered = concurrency.event_semaphore()
  let ordered =
    concurrency.ready(ordered, fn(_semaphore) {
      io.println("lib-concurrency first waiter passed")
    })
  let ordered =
    concurrency.ready(ordered, fn(_semaphore) {
      io.println("lib-concurrency second waiter passed")
    })
  let _ = concurrency.set(ordered)

  let semaphore = concurrency.event_semaphore()
  let semaphore =
    concurrency.ready(semaphore, fn(semaphore) {
      let semaphore = concurrency.clear(semaphore)
      let semaphore = concurrency.clear(semaphore)
      let semaphore =
        concurrency.ready(semaphore, fn(_semaphore) {
          io.println("lib-concurrency Promise semaphore parity passed")
        })
      let _ = concurrency.set(semaphore)
      Nil
    })
  let semaphore = concurrency.set(semaphore)
  let semaphore = concurrency.clear(semaphore)
  let _ = concurrency.set(semaphore)
}
