open! Base
open Await_kernel
open Portable_kernel

type 'a inner =
  { (* The first cache line is for the consumer. It has 7 fields, plus a header word *)
    mutable rd_index : int [@atomic]
  ; mutable wr_index_cache : int
  ; rd_contents : 'a portended or_null array
  ; rd_capacity : int
  ; mutable wr_trigger : Trigger.Source.t [@atomic]
  ; _pad06 : int
  ; _pad07 : int
  ; (* The second cache line is for the producer. It has 8 fields: *)
    mutable wr_index : int [@atomic]
  ; wr_capacity : int
  ; mutable rd_index_cache : int
  ; wr_contents : 'a portended or_null array
  ; mutable rd_trigger : Trigger.Source.t [@atomic]
  ; _pad13 : int
  ; _pad14 : int
  ; _pad15 : int
  }

type ('a : value mod non_float, 'phantom) t = { inner : 'a inner @@ aliased contended }
[@@unboxed]

type ('a : value mod non_float, 'phantom) queue = ('a, 'phantom) t

module Producer = struct
  type 'a t = ('a, [ `Producer ]) queue
end

module Consumer = struct
  type 'a t = ('a, [ `Consumer ]) queue

  external magic_of_producer
    :  'a Producer.t Capsule.Owned.t @ local
    -> 'a t Capsule.Owned.t @ unique
    @@ portable
    = "%identity"
end

let create ~(capacity : int) =
  if capacity <= 0
  then invalid_arg "[Bounded_spsc_queue.create]: capacity must be strictly positive";
  let producer =
    Capsule.Owned.create (fun () ->
      let contents = Array.create ~len:(Int.ceil_pow2 capacity) Null in
      { inner =
          { rd_index = 0
          ; wr_index_cache = 0
          ; wr_trigger = Trigger.(source (create ()))
          ; rd_contents = contents
          ; rd_capacity = capacity
          ; _pad06 = 0
          ; _pad07 = 0
          ; wr_index = 0
          ; rd_index_cache = 0
          ; rd_trigger = Trigger.(source (create ()))
          ; wr_contents = contents
          ; wr_capacity = capacity
          ; _pad13 = 0
          ; _pad14 = 0
          ; _pad15 = 0
          }
      })
  in
  let consumer = Consumer.magic_of_producer (borrow_ producer) in
  #(producer, consumer)
;;

let%test_unit "['a t] is 15 words (16 including the header)" =
  let #(producer, _consumer) = create ~capacity:1 in
  let producer = Capsule.Owned.unwrap producer in
  [%test_eq: int] 15 (Obj.size (Obj.repr producer))
;;

(* Duplicated to make sure we only read from the reader's cache line in the reader, and
   the writer's cache line in the writer. These fields will have the same value. *)
let wr_mask t = Array.length t.inner.wr_contents - 1
let rd_mask t = Array.length t.inner.rd_contents - 1

type 'a pushed_or_full =
  | Pushed
  | Full of 'a
[@@or_null]

(** Try to push a value to the tail (producer end) of the queue. Returns [Pushed] if the
    value was pushed, or [Full value] if the queue was full, containing the value that was
    passed *)
let try_push t a =
  let current_wr_index = Atomic.Loc.get [%atomic.loc t.inner.wr_index] in
  (* SAFETY: It is safe to [Obj.magic_uncontended] to read and write
     [t.inner.rd_index_cache] here because only the producer can touch
     [t.inner.rd_index_cache] *)
  if current_wr_index
     = (Obj.magic_uncontended t.inner).rd_index_cache + t.inner.wr_capacity
     &&
     let head = Atomic.Loc.get [%atomic.loc t.inner.rd_index] in
     (Obj.magic_uncontended t.inner).rd_index_cache <- head;
     current_wr_index = head + t.inner.wr_capacity
  then Full a
  else (
    let idx = current_wr_index land wr_mask t in
    let arr = (Obj.magic_uncontended t.inner).wr_contents in
    [%debug assert (idx < Array.length arr)];
    Array.unsafe_set arr idx (This { portended = a });
    Atomic.Loc.set [%atomic.loc t.inner.wr_index] (current_wr_index + 1);
    Pushed)
;;

type ('a, 'r) res =
  | Terminated : ('a, 'a) res
  | Or_canceled : ('a, 'a Or_canceled.t) res

let[@inline] push_success (type r) t (res : (unit, r) res) : r =
  (* Upon successfully pushing to the queue, signal a sleeping consumer (if one exists). *)
  Trigger.Source.signal (Atomic.Loc.get [%atomic.loc t.inner.rd_trigger]);
  match res with
  | Or_canceled -> Completed ()
  | Terminated -> ()
;;

let[@inline never] rec push_slow_path
  : type (a : value mod non_float) r.
    Await.t @ local
    -> Cancellation.t @ local
    -> (a, _) t @ local
    -> a @ contended once portable unique
    -> (unit, r) res
    -> r
  =
  fun w cancellation t a res ->
  let a =
    (* SAFETY: The API ensures that the value is restricted to [once] when it is read out
       of the queue *)
    (Obj.magic_many [@mode portable contended unique]) a
  in
  (* Pushing failed; go to sleep until the consumer calls [pop] *)
  let trigger = Trigger.create () in
  Atomic.Loc.set [%atomic.loc t.inner.wr_trigger] (Trigger.source trigger);
  (* Once the trigger was installed, try to push again (in case a consumer raced us) *)
  match try_push t a with
  | Pushed ->
    (* A consumer raced us going to sleep! We can just return (because [try_push]
       succeeded) *)
    push_success t res [@nontail]
  | Full a ->
    (* Now we can actually go to sleep *)
    Await.await_until_terminated_or_canceled w cancellation trigger;
    if Await.is_terminated w
    then raise Await.Terminated
    else (
      match res with
      | Or_canceled ->
        if Cancellation.Expert.is_canceled_ignore_termination cancellation
        then Canceled
        else
          (* No need for a backoff here; we just got woken up from an await *)
          push_slow_path w cancellation t a res
      | Terminated ->
        (* No need for a backoff here; we just got woken up from an await *)
        push_slow_path w cancellation t a res)
;;

let[@inline] push_fast_path w cancellation t a res =
  let a =
    (* SAFETY: The API ensures that the value is restricted to [once] when it is read out
       of the queue *)
    (Obj.magic_many [@mode portable contended unique]) a
  in
  match try_push t a with
  | Pushed -> push_success t res [@nontail]
  | Full a -> push_slow_path w cancellation t a res
;;

let push w t a = push_fast_path w Cancellation.never t a Terminated
let push_or_cancel w c t a = push_fast_path w c t a Or_canceled

(* *)

(** Try to pop a value off the head (consumer end) of the queue. Returns [This value] if a
    value was popped, or [Null] if the queue was empty *)
let try_pop t =
  let current_rd_index = Atomic.Loc.get [%atomic.loc t.inner.rd_index] in
  (* SAFETY: It is safe to [Obj.magic_uncontended] to read and write
     [t.inner.wr_index_cache] here because only the consumer can touch
     [t.inner.wr_index_cache] *)
  if (Obj.magic_uncontended t.inner).wr_index_cache = current_rd_index
     &&
     let tail = Atomic.Loc.get [%atomic.loc t.inner.wr_index] in
     (Obj.magic_uncontended t.inner).wr_index_cache <- tail;
     tail = current_rd_index
  then Null
  else (
    let index = current_rd_index land rd_mask t in
    let value = (Obj.magic_uncontended t.inner.rd_contents).(index) in
    (Obj.magic_uncontended t.inner.rd_contents).(index) <- Null;
    Atomic.Loc.set [%atomic.loc t.inner.rd_index] (current_rd_index + 1);
    value)
;;

let[@inline] pop_success (type a r) t (value : a @ contended portable) (res : (a, r) res)
  : r @ contended once portable unique
  =
  (* Upon successfully popping from the queue, remove and signal a sleeping producer (if
     one exists). *)
  Trigger.Source.signal (Atomic.Loc.get [%atomic.loc t.inner.wr_trigger]);
  let value =
    (* SAFETY: Every value is pushed in the queue at unique, and only read from the queue
       once *)
    (Obj.magic_unique [@mode contended portable]) value
  in
  match res with
  | Or_canceled -> Completed value
  | Terminated -> value
;;

let[@inline never] rec pop_slow_path
  : type (a : value mod non_float) r.
    _ @ local
    -> _ @ local
    -> (a, _) t @ local
    -> (a, r) res
    -> r @ contended once portable unique
  =
  fun w cancellation t res ->
  (* Popping failed; go to sleep until the producer calls [push] *)
  let trigger = Trigger.create () in
  Atomic.Loc.set [%atomic.loc t.inner.rd_trigger] (Trigger.source trigger);
  (* Once the trigger was installed, try to pop again (in case a producer raced us) *)
  match try_pop t with
  | This { portended = value } ->
    (* A producer raced us going to sleep! We can just return the (successful) result of
       the pop. *)
    pop_success t value res [@nontail]
  | Null ->
    (* Now we can actually go to sleep *)
    Await.await_until_terminated_or_canceled w cancellation trigger;
    if Await.is_terminated w
    then raise Await.Terminated
    else (
      match res with
      | Or_canceled ->
        if Cancellation.Expert.is_canceled_ignore_termination cancellation
        then Canceled
        else
          (* No need for a backoff here; we just got woken up from an await *)
          pop_slow_path w cancellation t res
      | Terminated ->
        (* No need for a backoff here; we just got woken up from an await *)
        pop_slow_path w cancellation t res)
;;

let[@inline] pop_fast_path w cancellation t res =
  match try_pop t with
  | This { portended = value } -> pop_success t value res [@nontail]
  | Null -> pop_slow_path w cancellation t res
;;

let pop w t = pop_fast_path w Cancellation.never t Terminated
let pop_or_cancel w c t = pop_fast_path w c t Or_canceled

(* *)

let capacity t = t.inner.rd_capacity
let equal a b = phys_equal a.inner b.inner
