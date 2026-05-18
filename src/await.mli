include sig
  open Await_kernel
  module Cancellation = Cancellation
  module Or_canceled = Or_canceled
  module Or_would_block = Or_would_block
  module Terminator = Terminator
  module Trigger = Trigger
  module Yield = Yield

  exception Poisoned
  exception Frozen
  exception Empty
  exception Already_full
end

module Await : sig
  (** @inline *)
  include module type of struct
    include Await_kernel.Await
  end

  (** @inline *)
  include module type of struct
    include Await_sync.Await
  end
end

module Sync : sig
  (** @inline *)
  include module type of struct
    include Await_kernel.Sync
  end

  (** [Sync.blocking] is an implementation of synchronization that blocks the current OS
      thread by coordinating with the operating system. On Linux, this is currently
      implemented by waiting on a futex.

      [Sync.blocking] is a reasonable choice of [Sync.t] in cases where you expect locks
      to be uncontended or rarely contended, and do not otherwise have easy access to a
      Sync.t. If you are running in a scheduler such as the [Parallel] scheduler, however,
      you should prefer using the [Sync.t] provided to you by the scheduler (eg by calling
      [Parallel.sync]) *)
  val blocking : t

  (** [Sync.spinning] is an implementation of synchronization that blocks the current OS
      thread by spinning.

      [Sync.spinning] is usually a poor choice of [Sync.t], but it can be useful for short
      and rarely contended critical sections that should never suspend execution. *)
  val spinning : t

  (** @inline *)
  include module type of struct
    include Await_sync.Sync
  end
end

module Capsule = Await_capsule
module Scratchpad = Await_scratchpad
