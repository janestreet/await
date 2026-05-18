open! Base
open Await_kernel

let sync #({ portended = () }, { global = trigger }) =
  while not (Trigger.is_signalled trigger) do
    Basement.Stdlib_shim.Domain.cpu_relax ()
  done
;;

let sync = Sync.create ~sync ~yield:Null ()
