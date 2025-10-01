open Await_kernel

let with_await terminator ~f =
  let await () trigger =
    while not (Trigger.is_signalled trigger) do
      Basement.Stdlib_shim.Domain.cpu_relax ()
    done
  in
  Await.with_ ~terminator ~await ~f ~yield:Null () [@nontail]
;;
