open Base
open Await

module Context = struct
  type t =
    { mutex : Stdlib.Mutex.t Portable_lazy.t @@ global
    ; condition : Stdlib.Condition.t Portable_lazy.t @@ global
    }

  let create () =
    { mutex = Portable_lazy.from_fun (fun () -> Stdlib.Mutex.create ())
    ; condition = Portable_lazy.from_fun (fun () -> Stdlib.Condition.create ())
    }
  ;;
end

let wakeup mutex condition =
  let thunk () =
    (match Stdlib.Mutex.lock mutex with
     | () -> Stdlib.Mutex.unlock mutex
     | exception Sys_error _ -> ());
    Stdlib.Condition.broadcast condition
  in
  thunk
;;

let await context trigger =
  let mutex = Portable_lazy.force context.Context.mutex in
  let condition = Portable_lazy.force context.Context.condition in
  if Trigger.on_signal trigger (wakeup mutex condition)
  then (
    (* NOTE: This doesn't use [Stdlib.Mutex.protect] only to avoid a heap allocation for
       the closure. *)
    Stdlib.Mutex.lock mutex;
    match
      while not (Trigger.is_signalled trigger) do
        Stdlib.Condition.wait condition mutex
      done
    with
    | () -> Stdlib.Mutex.unlock mutex
    | exception exn ->
      let bt = Backtrace.Exn.most_recent () in
      Stdlib.Mutex.unlock mutex;
      Exn.raise_with_original_backtrace exn bt)
;;

let with_await terminator ~f =
  (* We allocate [mutex] and [condition] lazily to make [run_with_await] as low overhead
     as possible. Sometimes they are not needed as nothing actually needs to block. *)
  let context = Context.create () in
  let await = Await.create terminator ~await context in
  f await [@nontail]
;;
