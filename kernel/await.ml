open Base
include Await_kernel_intf

type t : value mod contended portable =
  { sync : Sync.t
  ; terminator : Terminator.t
  }

let%template create ~sync ~terminator = { sync; terminator } [@exclave_if_stack a]
[@@alloc a @ l = (stack_local, heap_global)]
;;

let terminator { terminator; _ } = terminator
let sync { sync; _ } = sync

let call_sync { sync; _ } trigger =
  Sync.Expert.sync_without_checking_trigger sync ~on:trigger
;;

let await t ~on_terminate ~on:trigger =
  if not (Trigger.is_signalled trigger)
  then (
    (match Terminator.add_trigger t.terminator on_terminate with
     | Terminated -> Trigger.Source.signal on_terminate
     | Attached | Signaled -> ());
    call_sync t trigger [@nontail])
;;

let await_until_terminated t trigger =
  match Terminator.add_trigger t.terminator (Trigger.source trigger) with
  | Attached -> call_sync t trigger
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
  if not (Trigger.is_signalled trigger) then call_sync t trigger
;;

let await_with_terminate t trigger ~terminate r =
  if not (Trigger.is_signalled trigger)
  then (
    let on_terminate = Trigger.create_with_action ~f:terminate r in
    match Terminator.add_trigger t.terminator (Trigger.source on_terminate) with
    | Terminated ->
      Trigger.Source.signal (Trigger.source on_terminate);
      call_sync t trigger
    | Attached | Signaled ->
      call_sync t trigger;
      let _ : bool = Trigger.drop on_terminate in
      ())
;;

let await_with_terminate_or_cancel t cancellation trigger ~terminate_or_cancel request =
  if not (Trigger.is_signalled trigger)
  then (
    let on_terminate_or_cancel =
      Trigger.create_with_action ~f:terminate_or_cancel request
    in
    match
      Cancellation.add_trigger cancellation (Trigger.source on_terminate_or_cancel)
    with
    | Canceled ->
      Trigger.Source.signal (Trigger.source on_terminate_or_cancel);
      call_sync t trigger
    | Attached | Signaled ->
      (match
         Terminator.add_trigger t.terminator (Trigger.source on_terminate_or_cancel)
       with
       | Terminated ->
         Trigger.Source.signal (Trigger.source on_terminate_or_cancel);
         call_sync t trigger
       | Attached | Signaled ->
         call_sync t trigger;
         let _ : bool = Trigger.drop on_terminate_or_cancel in
         ()))
;;

let is_terminated t = Terminator.is_terminated t.terminator [@nontail]
let with_terminator t terminator = exclave_ { t with terminator }
let check_terminated t = Terminator.check (terminator t) [@nontail]

let yield t =
  check_terminated t;
  Sync.yield (sync t) [@nontail]
;;

let is_canceled t c = Cancellation.is_canceled c ~terminator:(terminator t) [@nontail]
let check_canceled t c = Cancellation.check c ~terminator:(terminator t) [@nontail]

module For_testing = struct
  let never = create ~sync:Sync.For_testing.never ~terminator:Terminator.unkillable
end

module Expert = struct
  let%template create = (create [@alloc a]) [@@alloc a @ l = (stack_local, heap_global)]

  let%template with_sync { terminator; _ } sync =
    (create [@alloc a]) ~terminator ~sync [@exclave_if_stack a]
  [@@alloc a @ l = (stack_local, heap_global)]
  ;;
end
