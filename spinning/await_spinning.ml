open! Base
open Await_kernel

let with_sync ~f =
  let sync #((), { global = trigger }) =
    while not (Trigger.is_signalled trigger) do
      Basement.Stdlib_shim.Domain.cpu_relax ()
    done
  in
  (Sync.with_
     ~sync
     ~f:(fun [@inline] w -> { aliased_many = { global = (f [@inlined hint]) w } })
     ~yield:Null
     ())
    .aliased_many
    .global
;;

let with_await terminator ~f =
  with_sync ~f:(fun [@inline] sync ->
    f ((Await.Expert.create [@alloc stack]) ~sync ~terminator) [@nontail])
  [@nontail]
;;
