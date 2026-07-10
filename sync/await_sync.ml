(** Basic synchronization primitives using [Await]. *)

include Await_sync_intf

(** Synchronization primitives which use {!Sync.t} *)
module Sync = struct
  module Mutex = Mutex.Sync
  module Rwlock = Rwlock.Sync
  module Lazy = Lazy.Sync
end

(** Synchronization primitives which use {!Await.t} *)
module Await = struct
  module Atom = Atom
  module Awaitable = Awaitable
  module Barrier = Barrier
  module Countdown_latch = Countdown_latch
  module Ivar = Ivar
  module Lazy = Lazy.Await
  module Mutex = Mutex.Await
  module Bounded_spsc_queue = Bounded_spsc_queue
  module Mpmc_queue = Mpmc_queue
  module Mvar = Mvar
  module Rwlock = Rwlock.Await
  module Scope = Scope
  module Semaphore = Semaphore
  module Stack = Stack
end

(**/**)

module Expert = struct
  module Lazy = struct
    module type S = Lazy.S

    include Lazy.Expert
  end

  module Parking_lot = Parking_lot
end
