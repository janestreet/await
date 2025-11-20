open Base
open Await_kernel
open Basement
open Capsule.Blocking_sync [@@alert "-deprecated"]

module Context = struct
  (* We allocate [mutex] and [condition] lazily to make [with_await] as low overhead as
     possible. Sometimes they are not needed as nothing actually needs to block. *)
    type%fuelproof inner : value mod contended portable =
      | T :
          { mutex : 'k Mutex.t
          ; condition : 'k Condition.t
          }
          -> inner

  let create_inner () =
    let (P key) = Capsule.Expert.create () in
    let mutex = Mutex.create key in
    let condition = Condition.create () in
    let inner = T { mutex; condition } in
    inner
  ;;

  type t = { mutable inner : inner or_null }

  let create () = exclave_ { inner = Null }
end

type t = { context : Context.inner @@ aliased global many } [@@unboxed]

let wakeup { context = T { mutex; condition } } =
  (try Mutex.with_lock mutex ~f:(fun _ -> ()) with
   | Mutex.Poisoned | Sys_error _ -> ());
  Condition.broadcast condition
;;

module TLS = Stdlib_shim.Domain.Safe.TLS

let inner_key = TLS.new_key Context.create_inner

let await (context : Context.t) trigger =
  let context =
    match context.inner with
    | Null ->
      let inner = TLS.get inner_key in
      context.inner <- This inner;
      inner
    | This inner -> inner
  in
  let (T { mutex; condition }) = context in
  match Trigger.on_signal trigger ~f:wakeup { context } with
  | Null ->
    let rec wait key =
      if Trigger.is_signalled trigger
      then #((), key)
      else (
        let key = Condition.wait condition ~mutex key in
        wait key)
    in
    Mutex.with_key mutex ~f:wait [@nontail]
  | This _ -> ()
;;

let yield _ = yield ()

let with_await terminator ~f =
  let context = Context.create () in
  Await.with_ ~terminator ~await ~yield:(This yield) context ~f [@nontail]
;;
