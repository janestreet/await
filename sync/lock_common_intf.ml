open! Base
open Await_kernel

module type Capability = sig @@ portable
  type t : value mod contended portable

  val unsafe_to_await : t @ local -> Await.t @ local
end

module type Lock_common = sig @@ portable
  module type Capability = Capability

  module Await : Capability with type t = Await.t
  module Sync : Capability with type t = Sync.t
end
