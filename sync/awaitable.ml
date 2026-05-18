open! Base
open! Import

(** See [Adaptive_backoff.once] *)
let log_scale = 10

(** See [Adaptive_backoff.once] *)
let log_scale_for_queue = 11

module Queue : sig @@ portable
  type 'a t : immutable_data with 'a

  val empty : unit -> 'a t
  val length : 'a t -> int
  val add : 'a -> 'a t -> 'a t
  val filter : f:('a -> bool) @ local -> 'a t -> 'a t

  module All_or_first : sig
    type t =
      | All
      | First
  end

  module Keep_or_return_or_drop : sig
    type t =
      | Keep
      | Return
      | Drop
  end

  val split
    :  f:('a -> Keep_or_return_or_drop.t) @ local
    -> All_or_first.t
    -> 'a t
    -> 'a t * 'a list
end = struct
  type 'a t = 'a list

  let empty () = []
  let length xs = List.length xs
  let add x xs = xs @ [ x ]
  let filter ~f xs = List.filter ~f xs

  module All_or_first = struct
    type t =
      | All
      | First
  end

  module Keep_or_return_or_drop = struct
    type t =
      | Keep
      | Return
      | Drop
  end

  let split ~f all_or_first xs =
    let rec loop f all_or_first ~keep ~return = function
      | [] -> List.rev keep, List.rev return
      | x :: xs ->
        (match (f x : Keep_or_return_or_drop.t) with
         | Keep -> loop f all_or_first ~keep:(x :: keep) ~return xs
         | Drop -> loop f all_or_first ~keep ~return xs
         | Return ->
           let return = x :: return in
           (match (all_or_first : All_or_first.t) with
            | All -> loop f all_or_first ~keep ~return xs
            | First -> List.rev_append keep xs, return))
    in
    loop f all_or_first ~keep:[] ~return:[] xs
  ;;
end

type ('a : value_or_null) awaiter =
  { comparand : 'a @@ contended portable
  ; trigger : Trigger.Source.t
  }

type ('a : value_or_null) awaitable =
  { mutable value : 'a portended [@atomic]
  ; mutable random_key : int or_null [@atomic]
  }

type ('a : value_or_null) t = { inner : 'a awaitable @@ contended global } [@@unboxed]

let[@inline never] random_key t =
  let key = Int64.to_int_trunc (Random.bits64 ()) in
  match
    Atomic.Loc.compare_exchange
      [%atomic.loc t.inner.random_key]
      ~if_phys_equal_to:Null
      ~replace_with:(This key)
  with
  | Null -> key
  | This key -> key
;;

let[@inline] random_key t =
  match Atomic.Loc.get [%atomic.loc t.inner.random_key] with
  | Null -> random_key t
  | This key -> key
;;

let[@inline] random_key_for_queue t = random_key { inner = t } + 0x80

type packed_awaitable =
  | Awaitable : ('a : value_or_null). 'a awaitable -> packed_awaitable
[@@unboxed]

type packed_awaiter = Awaiter : ('a : value_or_null). 'a awaiter -> packed_awaiter
[@@unboxed]

let awaiters : (packed_awaitable, packed_awaiter Queue.t) Htbl.t =
  Htbl.create
    (module struct
      type t = packed_awaitable

      let hash (Awaitable t) = random_key { inner = t }
      let equal = phys_equal
    end)
;;

let[@inline] make ?padded value =
  { inner = Padding.copy_as ?padded { value = { portended = value }; random_key = Null } }
;;

let[@inline] get t = (Atomic.Loc.get [%atomic.loc t.inner.value]).portended

let[@inline] compare_and_set t ~if_phys_equal_to ~replace_with =
  Atomic.Loc.compare_and_set
    [%atomic.loc t.inner.value]
    ~if_phys_equal_to:{ portended = if_phys_equal_to }
    ~replace_with:{ portended = replace_with }
;;

module Compare_failed_or_set_here = Atomic.Compare_failed_or_set_here

let[@inline] compare_exchange t ~if_phys_equal_to ~replace_with =
  (Atomic.Loc.compare_exchange
     [%atomic.loc t.inner.value]
     ~if_phys_equal_to:{ portended = if_phys_equal_to }
     ~replace_with:{ portended = replace_with })
    .portended
;;

let[@inline] exchange t value =
  (Atomic.Loc.exchange [%atomic.loc t.inner.value] { portended = value }).portended
;;

(* This is safe because [portended] is [[@@unboxed]], meaning [int] and [int portended]
   have the same runtime representation. *)
external fetch_and_add_portended_loc
  :  (int portended Atomic.Loc.t[@local_opt]) @ contended
  -> int
  -> int
  @@ portable
  = "%atomic_fetch_add_loc"

let[@inline] fetch_and_add t n = fetch_and_add_portended_loc [%atomic.loc t.inner.value] n

let[@inline] set t value =
  Atomic.Loc.set [%atomic.loc t.inner.value] { portended = value }
;;

let[@inline] incr t = ignore (fetch_and_add t 1 : int)
let[@inline] decr t = ignore (fetch_and_add t (-1) : int)

let[@inline] get_and_update t ~pure_f =
  let[@inline] rec aux () =
    let old = get t in
    let new_ = pure_f old in
    match compare_and_set t ~if_phys_equal_to:old ~replace_with:new_ with
    | Set_here -> old
    | Compare_failed ->
      Adaptive_backoff.once ~random_key:(random_key t) ~log_scale;
      aux ()
  in
  aux () [@nontail]
;;

let[@inline] update (type a : value_or_null) (t : a t) ~pure_f =
  Basement.Stdlib_shim.ignore_contended (get_and_update t ~pure_f : a)
;;

type _ await =
  | Signaled : [> `Signaled ] await
  | Terminated : [> `Terminated ] await
  | Canceled : [> `Canceled ] await

let rec add t awaiter =
  match Htbl.find awaiters (Awaitable t) with
  | Null ->
    let after = Queue.add (Awaiter awaiter) (Queue.empty ()) in
    (match Htbl.add awaiters ~key:(Awaitable t) ~data:after with
     | Null -> ()
     | This _ ->
       Adaptive_backoff.once
         ~random_key:(random_key_for_queue t)
         ~log_scale:log_scale_for_queue;
       add t awaiter)
  | This before ->
    let after = Queue.add (Awaiter awaiter) before in
    let actual =
      Htbl.compare_exchange
        awaiters
        (Awaitable t)
        ~if_phys_equal_to:before
        ~replace_with:after
    in
    if not (phys_equal actual (This before))
    then (
      Adaptive_backoff.once
        ~random_key:(random_key_for_queue t)
        ~log_scale:log_scale_for_queue;
      add t awaiter)
;;

let rec remove_signalled t =
  match Htbl.find awaiters (Awaitable t) with
  | Null -> ()
  | This before ->
    let after =
      Queue.filter
        ~f:(fun (Awaiter awaiter) -> not (Trigger.Source.is_signalled awaiter.trigger))
        before
    in
    let actual =
      if phys_equal after (Queue.empty ())
      then Htbl.compare_remove awaiters (Awaitable t) ~if_phys_equal_to:before
      else
        Htbl.compare_exchange
          awaiters
          (Awaitable t)
          ~if_phys_equal_to:before
          ~replace_with:after
    in
    if not (phys_equal actual (This before))
    then (
      Adaptive_backoff.once
        ~random_key:(random_key_for_queue t)
        ~log_scale:log_scale_for_queue;
      remove_signalled t)
;;

let rec resume t all_or_first =
  match Htbl.find awaiters (Awaitable t) with
  | Null -> ()
  | This before ->
    let value = (Atomic.Loc.get [%atomic.loc t.value]).portended in
    let after, to_signal =
      Queue.split all_or_first before ~f:(fun (Awaiter awaiter) ->
        if Trigger.Source.is_signalled awaiter.trigger
        then Drop
        else if (* [awaiters] is keyed by identity of awaitables -- the types match. *)
                phys_equal
                  (Obj.Nullable.(repr [@mode contended]) awaiter.comparand)
                  (Obj.Nullable.(repr [@mode contended]) value)
        then Keep
        else Return)
    in
    let actual =
      if phys_equal after (Queue.empty ())
      then Htbl.compare_remove awaiters (Awaitable t) ~if_phys_equal_to:before
      else
        Htbl.compare_exchange
          awaiters
          (Awaitable t)
          ~if_phys_equal_to:before
          ~replace_with:after
    in
    if phys_equal actual (This before)
    then
      List.iter
        ~f:(fun (Awaiter awaiter) -> Trigger.Source.signal awaiter.trigger)
        to_signal [@nontail]
    else (
      Adaptive_backoff.once
        ~random_key:(random_key_for_queue t)
        ~log_scale:log_scale_for_queue;
      resume t all_or_first)
;;

let signal t = resume t.inner First
let broadcast t = resume t.inner All

let await_or_cancel_as w cancellation t comparand on_canceled =
  let trigger = Trigger.create () in
  let awaiter = { comparand; trigger = Trigger.source trigger } in
  (* We assume that the caller has just a couple of nanoseconds earlier obtained the
     [comparand] value from the awaitable and so we don't bother to do the equality test
     before adding the [awaiter]. *)
  add t.inner awaiter;
  (* Now that the [awaiter] has been added to the queue we are guaranteed not to miss a
     signal, but it is possible that a signal happened concurrently with adding the
     awaiter and before suspending the task we must check whether the value of the
     awaitable is equal to [comparand] or not. *)
  if phys_equal (get t) comparand
  then (
    let forward t result =
      remove_signalled t.inner;
      signal t;
      result
    in
    Await.await_until_terminated_or_canceled w cancellation trigger;
    if Await.is_terminated w
    then forward t Terminated
    else if Cancellation.Expert.is_canceled_ignore_termination cancellation
    then forward t on_canceled
    else Signaled)
  else (
    Trigger.Source.signal (Trigger.source trigger);
    remove_signalled t.inner;
    Signaled)
;;

let await w t ~until_phys_unequal_to:comparand =
  await_or_cancel_as w Cancellation.never t comparand Terminated
;;

let await_or_cancel w c t ~until_phys_unequal_to:comparand =
  await_or_cancel_as w c t comparand Canceled
;;

module Awaiter = struct
  type t =
    | T :
        ('a : value_or_null).
        { awaitable : 'a awaitable @@ contended global
        ; awaiter : 'a awaiter @@ global
        }
        -> t

  let%template create_and_add t trigger ~until_phys_unequal_to:comparand =
    let awaiter = { comparand; trigger } in
    add t.inner awaiter;
    T { awaitable = t.inner; awaiter } [@exclave_if_local l]
  [@@mode l = (global, local)]
  ;;

  let cancel_and_remove (T { awaitable = t; awaiter }) =
    Trigger.Source.signal awaiter.trigger;
    remove_signalled t;
    signal { inner = t }
  ;;
end

module For_testing = struct
  let length { inner = t } =
    match Htbl.find awaiters (Awaitable t) with
    | Null -> 0
    | This queue -> Queue.length queue
  ;;
end
