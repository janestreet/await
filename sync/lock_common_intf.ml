open! Base
open Await_kernel

module type Capability = sig @@ portable
  type t : value mod contended portable

  val unsafe_to_await : t @ local -> Await.t @ local
end

module type Capability_with_of_await = sig @@ portable
  include Capability

  val of_await : Await.t @ local -> t @ local
end

module type Lock_common = sig @@ portable
  module type Capability = Capability
  module type Capability_with_of_await = Capability_with_of_await

  module Await : Capability_with_of_await with type t = Await.t
  module Sync : Capability_with_of_await with type t = Sync.t
end
