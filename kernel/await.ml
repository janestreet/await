open Base
open Portable_kernel

exception Terminated

let () =
  Stdlib.Printexc.Safe.register_printer (function
    | Terminated -> Some "Terminated"
    | _ -> None)
;;

type ('c, 'k) handler =
  { password : 'k Capsule.Expert.Password.t
  ; context : ('c, 'k) Capsule.Data.t
  ; await : ('c @ local -> Trigger.t -> unit, 'k) Capsule.Data.t
  ; yield : ('c @ local -> unit, 'k) Capsule.Data.t or_null
  }

type%fuelproof t : value mod contended portable =
  | Await :
      { terminator : Terminator.t
      ; handler : ('c, 'k) handler
      }
      -> t

let with_ ~terminator ~yield ~await context ~f =
  let (P access) = Capsule.Access.current () in
  let await = Capsule.Data.(wrap [@mode local]) ~access await in
  let yield =
    match yield with
    | Null -> Null
    | This yield -> This (Capsule.Data.(wrap [@mode local]) ~access yield)
  in
  let context = Capsule.Data.(wrap [@mode local]) ~access context in
  (Capsule.Expert.Password.with_current access (fun password ->
     let await = Await { terminator; handler = { yield; await; context; password } } in
     { aliased = { global = f await } }))
    .aliased
    .global
;;

let terminator (Await { terminator; _ }) = terminator

let[@inline] call_await
  (Await { handler = { await; context; password; yield = _ }; terminator = _ })
  (trigger : Trigger.t)
  =
  Capsule.Expert.Data.Local.iter
    ~password
    (Capsule.Expert.Data.Local.both await context)
    ~f:(fun [@inline] (await, context) -> await context trigger [@nontail])
  [@nontail]
;;

let await (Await { terminator; _ } as t) ~on_terminate ~await_on =
  if not (Trigger.is_signalled await_on)
  then (
    (match Terminator.add_trigger terminator on_terminate with
     | Terminated -> Trigger.Source.signal on_terminate
     | Attached | Signaled -> ());
    call_await t await_on [@nontail])
;;

let await_until_terminated (Await { terminator; _ } as t) trigger =
  match Terminator.add_trigger terminator (Trigger.source trigger) with
  | Attached -> call_await t trigger
  | Terminated -> Trigger.Source.signal (Trigger.source trigger)
  | Signaled -> ()
;;

let await_until_terminated_or_canceled w cancellation trigger =
  match Cancellation.add_trigger cancellation (Trigger.source trigger) with
  | Attached -> await_until_terminated w trigger
  | Canceled -> Trigger.Source.signal (Trigger.source trigger)
  | Signaled -> ()
;;

let await_never_terminated t trigger =
  if not (Trigger.is_signalled trigger) then call_await t trigger
;;

external magic_many : 'a @ once portable -> 'a @ many portable @@ portable = "%identity"

let await_with_terminate (Await { terminator; _ } as t) trigger ~terminate =
  let terminate = magic_many terminate in
  if not (Trigger.is_signalled trigger)
  then (
    let on_terminate = Trigger.create_with_action ~f:terminate () in
    match Terminator.add_trigger terminator (Trigger.source on_terminate) with
    | Terminated ->
      terminate ();
      call_await t trigger
    | Attached | Signaled ->
      call_await t trigger;
      let _ : bool = Trigger.drop on_terminate in
      ())
;;

let is_terminated (Await { terminator; _ }) = Terminator.is_terminated terminator
let with_terminator (Await r) terminator = exclave_ Await { r with terminator }

let yield (Await { handler = { yield; password; context; await = _ }; terminator }) =
  if Terminator.is_terminated terminator then raise Terminated;
  match yield with
  | Null -> ()
  | This yield ->
    Capsule.Expert.Data.Local.iter
      ~password
      (Capsule.Expert.Data.Local.both yield context)
      ~f:(fun [@inline] (yield, context) -> yield context [@nontail])
    [@nontail]
;;

module For_testing = struct
  let with_never ~f =
    let await () _ =
      failwith
        "[await never] was called. Usually this means that an operation blocked which \
         was expected to never block"
    in
    let (P access) = Capsule.Access.current () in
    let await = Capsule.Data.(wrap [@mode local]) ~access await in
    let context = Capsule.Data.(wrap [@mode local]) ~access () in
    (Capsule.Expert.Password.with_current access (fun password ->
       let await =
         Await
           { terminator = Terminator.never
           ; handler = { await; context; password; yield = Null }
           }
       in
       { aliased = { global = f await } }))
      .aliased
      .global
  ;;
end
