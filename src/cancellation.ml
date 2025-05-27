open! Base
open! Portable

(* The underlying state machine of cancellation:

      [with_*]                           [never]
         |                                  |
         +------------------------+       Never <-----+
         |                        |         |         |
         v                        |         +-- [*] --+
      Nil/Cons -- [add_trigger] --+
         |
      [cancel]
         |
         v
      Canceled

   The [Canceled] and [Never] states are terminal. *)

type _ state_inner =
  | Never : [> `Never ] state_inner
  | Canceled : [> `Canceled ] state_inner
  | Nil : [> `Nil ] state_inner
  | Cons :
      { countdown : int
          (* [countdown] is used to amortize the cost of removing signalled triggers.

             Each time a [Cons _] is added for a new trigger, the [countdown] is
             decremented unless it was zero, in which case [cleanup] will be called
             instead.

             [cleanup unsignalled possibly_signalled] is called with a [Cons _] of a new
             [unsignalled] trigger to be added and it then goes through the list of
             [possibly_signalled] triggers to accumulate all the unsignalled triggers and
             count them as the value of the [countdown] in the [Cons _] of the accumulated
             list of unsignalled triggers.

             This amortizes the cost of cleaning up signalled triggers to to O(1) per
             trigger and also guarantees that no more than O(max n) space is used where
             [max n] is the maximum number of unsignalled triggers at any point.

             Also, the expectation is that triggers are signalled frequently relative to
             adding them. *)
      ; trigger : Trigger.Source.t
      ; next : [ `Nil | `Cons ] state_inner
      }
      -> [> `Cons ] state_inner

type state = S : [< `Never | `Canceled | `Nil | `Cons ] state_inner -> state [@@unboxed]
type t = state Atomic.t Modes.Global.t

let same = Base.phys_equal
let never = { global = Atomic.make (S Never) }
let always = { global = Atomic.make (S Canceled) }

module Source = struct
  type t = state Atomic.t

  let rec cancel t =
    match Atomic.get t with
    | S Never -> assert false
    | S Canceled -> ()
    | S ((Nil | Cons _) as before) ->
      (match
         Atomic.compare_and_set t ~if_phys_equal_to:(S before) ~replace_with:(S Canceled)
       with
       | Set_here ->
         let rec signal = function
           | Nil -> ()
           | Cons r ->
             Trigger.Source.signal r.trigger;
             signal r.next
         in
         signal before
       | Compare_failed -> cancel t)
  ;;

  let is_canceled t =
    match Atomic.get t with
    | S Canceled -> true
    | S (Never | Nil | Cons _) -> false
  ;;
end

let is_canceled (t : t) = Source.is_canceled t.global

module Link = struct
  type t =
    | Attached
    | Canceled
    | Signaled
  [@@deriving equal ~localize, sexp ~localize]
end

let rec add_trigger t trigger backoff : Link.t =
  match Atomic.get t with
  | S Never -> Attached
  | S Canceled -> Canceled
  | S (Nil as before) ->
    if Trigger.Source.is_signalled trigger
    then Signaled
    else (
      let after = Cons { countdown = 1; trigger; next = Nil } in
      match
        Atomic.compare_and_set t ~if_phys_equal_to:(S before) ~replace_with:(S after)
      with
      | Set_here -> Attached
      | Compare_failed -> add_trigger t trigger (Backoff.once backoff))
  | S (Cons r as before) ->
    if Trigger.Source.is_signalled trigger
    then Signaled
    else if 0 < r.countdown
    then (
      let after = Cons { countdown = r.countdown - 1; trigger; next = before } in
      match
        Atomic.compare_and_set t ~if_phys_equal_to:(S before) ~replace_with:(S after)
      with
      | Set_here -> Attached
      | Compare_failed -> add_trigger t trigger (Backoff.once backoff))
    else (
      let rec cleanup (Cons after_r as after : [ `Cons ] state_inner) = function
        | Nil -> after
        | Cons before_r ->
          if Trigger.Source.is_signalled before_r.trigger
          then cleanup after before_r.next
          else
            cleanup
              (Cons
                 { countdown = after_r.countdown + 1
                 ; trigger = before_r.trigger
                 ; next = after
                 })
              before_r.next
      in
      let after = cleanup (Cons { countdown = 1; trigger; next = Nil }) before in
      if Trigger.Source.is_signalled trigger
      then Signaled
      else (
        match
          Atomic.compare_and_set t ~if_phys_equal_to:(S before) ~replace_with:(S after)
        with
        | Set_here -> Attached
        | Compare_failed -> add_trigger t trigger (Backoff.once backoff)))
;;

let[@inline] add_trigger t trigger = add_trigger t.global trigger Backoff.default

let check_clean_and_close t =
  match Atomic.get t with
  | S (Never | Canceled) -> ()
  | S ((Nil | Cons _) as before) ->
    let rec check = function
      | Nil -> true
      | Cons r -> Trigger.Source.is_signalled r.trigger && check r.next
    in
    if check before
    then (
      let _ : state =
        Atomic.compare_exchange t ~if_phys_equal_to:(S before) ~replace_with:(S Canceled)
      in
      ())
    else failwith "Cancellation: unsignalled triggers leaked"
;;

let[@inline] is_cancellable t = not (same t never)
let[@inline] source t = if is_cancellable t then Some t.global else None

let with_linked_multi parents body =
  let source = Atomic.make (S Nil) in
  let local_ t = { global = source } in
  let cancel () = Source.cancel source in
  let trigger = Trigger.create_with_action cancel in
  let rec add_to_parents = function
    | parent :: parents ->
      (match add_trigger parent (Trigger.source trigger) with
       | Attached -> add_to_parents parents
       | (Canceled | Signaled) as result -> result)
    | [] -> Attached
  in
  (match add_to_parents parents with
   | Attached | Signaled -> ()
   | Canceled -> if Trigger.drop trigger then Source.cancel source);
  match body t with
  | result ->
    let _ : bool = Trigger.drop trigger in
    check_clean_and_close t.global;
    result
  | exception exn ->
    let bt = Backtrace.Exn.most_recent () in
    let _ : bool = Trigger.drop trigger in
    check_clean_and_close t.global;
    Exn.raise_with_original_backtrace exn bt
;;

let with_linked parent body = with_linked_multi [ parent ] body [@nontail]
let with_ body = with_linked_multi [] body

module Expert = struct
  let globalize { global } = { global }
end

module For_testing = struct
  let get_countdown t =
    match Atomic.get t.global with
    | S (Never | Canceled | Nil) -> 0
    | S (Cons r) -> r.countdown
  ;;
end
