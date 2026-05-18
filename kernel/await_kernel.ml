(** {1 Concrete primitives} *)

module Trigger = Trigger
module Cancellation = Cancellation
module Terminator = Terminator
module Or_canceled = Or_canceled
module Or_would_block = Or_would_block

(** {1 Abstract capabilities} *)

module Await = Await
module Sync = Sync
module Yield = Yield
