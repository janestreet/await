(** @inline *)
include struct
  open Await_kernel
  module Cancellation = Cancellation
  module Or_canceled = Or_canceled
  module Or_would_block = Or_would_block
  module Terminator = Terminator
  module Trigger = Trigger
  module Yield = Yield

  exception Poisoned = Await_sync.Poisoned
  exception Frozen = Await_sync.Frozen
  exception Empty = Await_sync.Empty
  exception Already_full = Await_sync.Already_full
end

module Await = struct
  include Await_kernel.Await
  include Await_sync.Await
end

module Sync = struct
  include Await_kernel.Sync
  include Await_sync.Sync

  let blocking = Sync_blocking.sync
  let spinning = Sync_spinning.sync
end

module Capsule = Await_capsule
module Scratchpad = Await_scratchpad
