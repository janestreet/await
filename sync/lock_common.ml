open! Base
open Await_kernel
include Lock_common_intf

module Sync = struct
  include Sync

  let unsafe_to_await t = exclave_
    (Await.Expert.create [@alloc stack]) ~sync:t ~terminator:Terminator.never
  ;;
end

module Await = struct
  include Await

  let unsafe_to_await = Fn.id
end
