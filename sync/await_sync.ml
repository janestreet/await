(** Basic synchronization primitives using [Await] *)

include Await_sync_intf
module Atom = Atom
module Awaitable = Awaitable
module Barrier = Barrier
module Countdown_latch = Countdown_latch
module Scope = Scope
module Stack = Stack
module Ivar = Ivar
module Mvar = Mvar
module Mutex = Mutex
module Rwlock = Rwlock
module Semaphore = Semaphore
