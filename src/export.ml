open! Base
open! Portable

exception Terminated

type ('c : value mod contended) inner =
  { terminator : Terminator.t
  ; await : 'c @ local -> Trigger.t -> unit
  ; context : 'c
  }

type t = Await : 'c inner -> t [@@unboxed]

let create (terminator @ local) ~(await @ local) (context @ local) = exclave_
  Await { terminator; await; context }
;;

let terminator (Await r @ local) = r.terminator

let await (Await r @ local) ~on_terminate ~await_on =
  if not (Trigger.is_signalled await_on)
  then (
    (match Terminator.add_trigger r.terminator on_terminate with
     | Terminated -> Trigger.Source.signal on_terminate
     | Attached | Signaled -> ());
    r.await r.context await_on)
;;

let await_until_terminated (Await r @ local) trigger =
  match Terminator.add_trigger r.terminator (Trigger.source trigger) with
  | Attached -> r.await r.context trigger
  | Terminated -> Trigger.Source.signal (Trigger.source trigger)
  | Signaled -> ()
;;

let await_until_terminated_or_canceled w cancellation trigger =
  match Cancellation.add_trigger cancellation (Trigger.source trigger) with
  | Attached -> await_until_terminated w trigger
  | Canceled -> Trigger.Source.signal (Trigger.source trigger)
  | Signaled -> ()
;;

let await_never_terminated (Await r @ local) trigger =
  if not (Trigger.is_signalled trigger) then r.await r.context trigger
;;

external magic_many : 'a @ once portable -> 'a @ many portable @@ portable = "%identity"

let await_with_terminate (Await r @ local) trigger ~terminate =
  let terminate = magic_many terminate in
  if not (Trigger.is_signalled trigger)
  then (
    let on_terminate = Trigger.create_with_action terminate in
    match Terminator.add_trigger r.terminator (Trigger.source on_terminate) with
    | Terminated ->
      terminate ();
      r.await r.context trigger
    | Attached | Signaled ->
      r.await r.context trigger;
      let _ : bool = Trigger.drop on_terminate in
      ())
;;

let is_terminated (Await r @ local) = Terminator.is_terminated r.terminator
let with_terminator (Await r) terminator = exclave_ Await { r with terminator }
