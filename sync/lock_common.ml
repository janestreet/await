open! Base
open Await_kernel
include Lock_common_intf

module Sync = struct
  include Sync

  let of_await w = exclave_ Await.sync w

  let unsafe_to_await t = exclave_
    (Await.Expert.create [@alloc stack]) ~sync:t ~terminator:Terminator.unkillable
  ;;
end

module Await = struct
  include Await

  let of_await = Fn.id
  let unsafe_to_await = Fn.id
end
