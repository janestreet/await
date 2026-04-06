open! Base
open Await_kernel
open Capsule_blocking_sync [@@alert "-deprecated"]
module Futex = Await_blocking_futex

let sync #({ portended = () }, { global = trigger }) =
  let futex = Futex.get () in
  match Trigger.on_signal trigger ~f:[%eta1 Futex.signal] futex with
  | Null ->
    let[@inline] rec loop count =
      if not (Trigger.is_signalled trigger)
      then
        (* We might spuriously wakeup even if [Futex.signal] has not been called, so we
           need to loop around again to check. *)
        loop (Futex.wait futex ~count)
    in
    loop (Futex.count futex)
  | This _ ->
    (* One way we might get here is if:

       1. we decide we want to wait on a trigger
       2. some other thread signals the trigger
       3. we enter [await], [get] the futex, then try to register the action for the
          trigger.
    *)
    ()
;;

let yield _ = yield ()
let sync = Sync.create ~sync ~yield:(This yield) ()
