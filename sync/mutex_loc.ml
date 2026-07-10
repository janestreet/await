open Base
open Import

(** See [Adaptive_backoff.once] *)
let log_scale = 10

module Prim : sig @@ portable
  module State : sig
    type t : immediate

    val make : unit -> t
  end

  type t = State.t Atomic.Loc.t

  include Mutex_intf.Prim with type t := t
end = struct
  module State : sig @@ portable
    type t : immediate

    val make : unit -> t

    (* *)

    val backoff_key : t -> int
    val parking_lot_key : t -> int

    (* *)

    val has_uncontended_awaiters : t -> bool
    val is_poisoned : t -> bool
    val is_locked : t -> bool

    (* *)

    val and_locked : t -> t
    val and_unlocked : t -> t

    (* *)

    val and_with_random_parking_lot_key : t -> t
    val and_without_parking_lot : t -> t

    (* *)

    val and_poisoned : t -> t

    (* *)

    val fetch_and_unlock : t Atomic.Loc.t @ local -> t
  end = struct
    type t = int

    (* The state looks like this:

       {v
         Bit: [    0 to ABn-1   |    ABn to n-5   | n-4   |   n-3  |    n-2   | n-1 ]
         Use: [ key for backoff | key for parking | queue | locked | poisoned |  0  ]
       v}

       AB is for adaptive backoff and ABn is the number of bits for its key.

       The rest of the bits for the parking lot key are allocated dynamically. *)

    let poisoned_bit = 1 lsl (Int.num_bits - 2)
    let lock_bit = poisoned_bit lsr 1
    let queue_bit = lock_bit lsr 1
    let backoff_key_mask = (1 lsl Adaptive_backoff.Random_key.num_bits) - 1
    let parking_lot_key_mask = lnot (-queue_bit)
    let extra_parking_lot_key_bits_mask = parking_lot_key_mask land lnot backoff_key_mask

    (* *)

    let[@inline] make () = Int64.to_int_trunc (Random.bits64 ()) land backoff_key_mask

    (* *)

    let[@inline] backoff_key t =
      (* Adaptive backoff does not use all the bits so we don't bother masking. *)
      t
    ;;

    let[@inline] parking_lot_key t = t land parking_lot_key_mask

    (* *)

    let[@inline] has_uncontended_awaiters t = t land queue_bit <> 0
    let[@inline] is_poisoned t = poisoned_bit <= t
    let[@inline] is_locked t = lock_bit <= t

    (* *)

    let[@inline] and_locked t = t lor lock_bit
    let[@inline] and_unlocked t = t land lnot lock_bit

    let[@inline] and_with_random_parking_lot_key t =
      t
      lor queue_bit
      lor (Int64.to_int_trunc (Random.bits64 ()) land extra_parking_lot_key_bits_mask)
    ;;

    let[@inline] and_without_parking_lot t =
      t land lnot (queue_bit lor extra_parking_lot_key_bits_mask)
    ;;

    (* *)

    let[@inline] and_poisoned t = t lor poisoned_bit

    (* *)

    let[@inline] fetch_and_unlock t = Atomic.Loc.fetch_and_add t (-lock_bit)
  end

  type t = State.t Atomic.Loc.t

  type ('a, _) result =
    | Value : ('a, 'a) result
    | Or_canceled : ('a, 'a Or_canceled.t) result

  let[@inline] completed_as : type r. (unit, r) result -> r = function
    | Value -> ()
    | Or_canceled -> Completed ()
  ;;

  let[@inline] canceled_as : type r. (unit, r) result -> r = function
    | Value -> ()
    | Or_canceled -> Canceled
  ;;

  let[@inline never] broadcast state =
    Or_null.iter
      ~f:(Nonempty_queue.iter ~f:Trigger.Source.signal)
      (Parking_lot.remove (State.parking_lot_key state));
    #()
  ;;

  let[@inline never] rec signal (t @ local) before =
    (* As we use a separate parking lot to store the queue of awaiters, there are
       potentially two pieces of state, the parking lot and the mutex word, to update. *)
    match Parking_lot.find (State.parking_lot_key before) with
    | Null ->
      (* Nothing to update as there is no queue in the parking lot. It must be the case
         that the queue was emptied and the mutex word was updated by some other thread
         after we decided we need to signal. *)
      #()
    | This queue_before ->
      let #(trigger, binding_after) = Nonempty_queue.dequeue queue_before in
      (match binding_after with
       | Null ->
         (* As there is only one awaiter in the queue, we will try to remove the entire
            queue.

            First we need to update the mutex word. *)
         let after = before |> State.and_without_parking_lot in
         if phys_equal
              before
              (Atomic.Loc.compare_exchange t ~if_phys_equal_to:before ~replace_with:after)
         then
           (* We managed to update the mutex word. This means the queue is now considered
              stale and it is our responsibility to remove the queue from the parking lot
              and signal all the triggers there. We expect there to be one, but more might
              have been added and so we must signal them all. *)
           broadcast before
         else #()
       | This queue_after ->
         (* As there are more awaiters in the queue, we only need to update the queue in
            the parking lot except when the mutex word has been updated. *)
         if phys_equal before (Atomic.Loc.get t)
         then (
           match
             Parking_lot.compare_and_set
               (State.parking_lot_key before)
               ~if_phys_equal_to:queue_before
               ~replace_with:queue_after
           with
           | Set_here ->
             Trigger.Source.signal trigger;
             #()
           | Compare_failed -> signal t before)
         else #())
  ;;

  (** Drop trigger and forward signal in case of termination or cancellation. *)
  let rec drop_trigger_and_forward_signal t parking_lot_key trigger =
    match Parking_lot.find parking_lot_key with
    | Null ->
      (* The queue is empty, which means we must have already been signaled. However, as
         the queue is empty, there is no need to forward the signal as nobody was queued
         after us. *)
      #()
    | This queue_before ->
      (match Nonempty_queue.reject_exn trigger queue_before with
       | Null ->
         (* The queue is about to become empty, which means we need to first update the
            mutex word to make the queue stale. *)
         let before = Atomic.Loc.get t in
         if State.parking_lot_key before = parking_lot_key
         then (
           match
             Atomic.Loc.compare_and_set
               t
               ~if_phys_equal_to:before
               ~replace_with:(before |> State.and_without_parking_lot)
           with
           | Set_here ->
             (* We managed to update the mutex word. This means the queue is now
                considered stale and it is our responsibility to remove the queue from the
                parking lot and signal all the triggers there. We expect there to be none,
                but more might have been added and so we must signal them all. *)
             broadcast before
           | Compare_failed -> drop_trigger_and_forward_signal t parking_lot_key trigger)
         else
           (* Apparently we lost the race to update the queue as the mutex word no longer
              uses the queue. This must mean that we have been signalled, but also that we
              don't need to forward the signal. *)
           #()
       | This queue_after ->
         (match
            Parking_lot.compare_and_set
              parking_lot_key
              ~if_phys_equal_to:queue_before
              ~replace_with:queue_after
          with
          | Set_here ->
            (* We have successfully removed our trigger from the queue before it was
               signaled through the mutex and are done. *)
            #()
          | Compare_failed ->
            (* We lost the race and must retry. *)
            drop_trigger_and_forward_signal t parking_lot_key trigger)
       | exception Nonempty_queue.Not_found ->
         (* We are not in the queue, which means we must have already been signaled
            through the mutex and need to forward the signal unless the mutex is locked or
            the queue is stale. *)
         let before = Atomic.Loc.get t in
         if (not (State.is_locked before))
            && State.parking_lot_key before = parking_lot_key
         then signal t before
         else #())
  ;;

  let rec acquire_contended ((_w, _c, t, r) as wctr) =
    (* We are here after reading the mutex state and failing to immediately acquire the
       mutex.

       For simplicity we re-read the mutex word. In most cases the mutex word should be in
       the cache and this is expected to be cheap. *)
    let before = Atomic.Loc.get t in
    if not (State.is_locked before)
    then (
      (* The mutex was unlocked and we now try to lock it. *)
      let locked = before |> State.and_locked in
      match
        Atomic.Loc.compare_and_set t ~if_phys_equal_to:before ~replace_with:locked
      with
      | Set_here -> completed_as r
      | Compare_failed -> acquire_contended wctr)
    else if State.is_poisoned before
    then raise Poisoned
    else (
      (* The mutex is locked. We now backoff and then try again. *)
      Adaptive_backoff.once ~random_key:(State.backoff_key before) ~log_scale;
      let before = Atomic.Loc.get t in
      if not (State.is_locked before)
      then (
        (* The mutex was unlocked while we spinned and we now try to lock it. *)
        let locked = before |> State.and_locked in
        match
          Atomic.Loc.compare_and_set t ~if_phys_equal_to:before ~replace_with:locked
        with
        | Set_here -> completed_as r
        | Compare_failed -> acquire_contended wctr)
      else if State.is_poisoned before
      then raise Poisoned
      else if (* We couldn't lock the mutex after backoff so we now try to add ourselves
                 to the queue and wait for a signal. *)
              not (State.has_uncontended_awaiters before)
      then (
        (* The mutex word indicates that there is no queue of awaiters. So, we try to
           allocate a new queue. *)
        let queued = before |> State.and_with_random_parking_lot_key in
        let trigger = Trigger.create () in
        let parking_lot_key = State.parking_lot_key queued in
        if Parking_lot.add_new parking_lot_key (Trigger.source trigger)
        then
          (* We have allocated a new queue. We now try to update the mutex word to tell
             others about the queue. *)
          if phys_equal before (Atomic.Loc.get t)
             && phys_equal
                  before
                  (Atomic.Loc.compare_exchange
                     t
                     ~if_phys_equal_to:before
                     ~replace_with:queued)
          then
            (* We managed to update the mutex word and can wait on the trigger. *)
            acquire_await #(wctr, parking_lot_key, { global = trigger })
          else (
            (* We lost the race to update the mutex word. The queue we added is now stale
               and we are responsible for removing it before we retry. It should not be
               possible for anyone to have entered the queue as its key was never
               published to others. *)
            let _ : _ = Parking_lot.remove parking_lot_key in
            acquire_contended wctr)
        else
          (* There was a collision, i.e. our random key was already in use, as we have not
             mutated any state we can simply retry. *)
          acquire_contended wctr)
      else (
        (* The mutex word indicates that there is a queue of awaiters. So, we will try to
           enqueue ourselves to the existing queue. *)
        let queued = before in
        let parking_lot_key = State.parking_lot_key queued in
        match Parking_lot.find parking_lot_key with
        | Null ->
          (* Apparently we lost the race to update the queue. The queue is only removed
             after the mutex word has been updated and so the mutex word must have been
             updated and we need to retry. *)
          acquire_contended wctr
        | This queue_before ->
          (* There is indeed a queue. We will try to enqueue ourselves to the queue. *)
          let trigger = Trigger.create () in
          let queue_after =
            Nonempty_queue.enqueue (Trigger.source trigger) queue_before
          in
          (* Before updating the queue in the parking lot we check that the queue has not
             been made stale. We read mutex word again... *)
          let before = Atomic.Loc.get t in
          if not (State.is_locked before)
          then (
            let locked = before |> State.and_locked in
            match
              Atomic.Loc.compare_and_set t ~if_phys_equal_to:before ~replace_with:locked
            with
            | Set_here -> completed_as r
            | Compare_failed -> acquire_contended wctr)
          else if (* ...and check that it hasn't changed. *)
                  not (phys_equal before queued)
          then acquire_contended wctr
          else (
            match
              Parking_lot.compare_and_set
                parking_lot_key
                ~if_phys_equal_to:queue_before
                ~replace_with:queue_after
            with
            | Set_here ->
              (* We managed to update the queue and can wait on the trigger. *)
              acquire_await #(wctr, parking_lot_key, { global = trigger })
            | Compare_failed ->
              (* We lost the race to update the queue and must retry. *)
              acquire_contended wctr)))

  (* Actually wait for a signal that the mutex has been released. *)
  and acquire_await
    #(((w, c, t, r) as wctr), (parking_lot_key : int), { global = trigger })
    =
    Await.await_until_terminated_or_canceled w c trigger;
    let trigger_source = Trigger.source trigger in
    if Await.is_terminated w
    then (
      let #() = drop_trigger_and_forward_signal t parking_lot_key trigger_source in
      raise Await.Terminated)
    else if Cancellation.Expert.is_canceled_ignore_termination c
    then (
      let #() = drop_trigger_and_forward_signal t parking_lot_key trigger_source in
      canceled_as r)
    else acquire_contended wctr
  ;;

  (** This slow path entrypoint is not inlined to keep [acquire] and [acquire_or_cancel]
      as small as possible. *)
  let[@inline never] acquire_contended_enter #(w, c, t, r) =
    acquire_contended (w, c, t, r) [@nontail]
  ;;

  let[@inline] acquire_as #(w, c, t, r) =
    let before = Atomic.Loc.get t in
    (* We cannot use fetch-and-add here, because we don't have bits to spare for overflow.

       Furthermore, as the lock is embedded in some user data structure, it can be
       beneficial to avoid writes to the cache line. Usually reads cause less interference
       for the owner and we want to optimize for the performance of the owner. *)
    if State.is_locked before
       || not
            (phys_equal
               before
               (Atomic.Loc.compare_exchange
                  t
                  ~if_phys_equal_to:before
                  ~replace_with:(before |> State.and_locked)))
    then
      (* We want to have just one call to [acquire_contended_enter] to minimize the amount
         of code generation after inlining. *)
      acquire_contended_enter #(w, c, t, r)
    else completed_as r
  ;;

  let[@inline] acquire w t = acquire_as #(w, Cancellation.never, t, Value)
  let[@inline] acquire_or_cancel w c t = acquire_as #(w, c, t, Or_canceled)

  let try_acquire t =
    let[@inline never] failed state =
      if State.is_poisoned state then raise Poisoned;
      (* We backoff to avoid issues due to tight retry loops that could otherwise cause
         significant performance degradation due to the added contention as the cache line
         becomes shared and writes must invalidate. *)
      Adaptive_backoff.once ~random_key:(State.backoff_key state) ~log_scale;
      false
    in
    let mutable prior = Atomic.Loc.get t in
    if State.is_locked prior
       ||
       let state = prior in
       prior
       <- Atomic.Loc.compare_exchange
            t
            ~if_phys_equal_to:state
            ~replace_with:(state |> State.and_locked);
       not (phys_equal state prior)
    then failed prior
    else true
  ;;

  let[@inline] release t =
    let locked = State.fetch_and_unlock t in
    assert%debug (State.is_locked locked);
    if State.has_uncontended_awaiters locked
    then (
      let before = locked |> State.and_unlocked in
      let #() = signal t before in
      ())
  ;;

  let[@inline never] release_and_reraise exn t =
    let bt = Backtrace.Exn.most_recent () in
    release t;
    Exn.raise_with_original_backtrace exn bt
  ;;

  let[@inline never] rec poison t =
    let before = Atomic.Loc.get t in
    if not (State.is_poisoned before)
    then (
      let poisoned = before |> State.and_poisoned |> State.and_without_parking_lot in
      match
        Atomic.Loc.compare_and_set t ~if_phys_equal_to:before ~replace_with:poisoned
      with
      | Set_here ->
        if State.has_uncontended_awaiters before
        then (
          let #() = broadcast before in
          ())
      | Compare_failed -> poison t)
  ;;

  let[@inline never] poison_and_reraise exn t =
    let bt = Backtrace.Exn.most_recent () in
    poison t;
    Exn.raise_with_original_backtrace exn bt
  ;;

  let is_poisoned t = State.is_poisoned (Atomic.Loc.get t)
  let is_locked t = State.is_locked (Atomic.Loc.get t)

  module For_testing = struct
    let is_exclusive = is_locked
    let length _t = Parking_lot.For_testing.non_linearizable_length ()
  end

  let create ?padded () =
    let open struct
      type t = { mutable state : State.t [@atomic] }
    end in
    let t = Portable_common.Padding.copy_as ?padded { state = State.make () } in
    [%atomic.loc t.state]
  ;;
end

module Sync = struct
  module State = struct
    type 'k t = Prim.State.t
  end

  include Mutex_common.Make_sync (Prim)

  let make _ = Prim.State.make ()

  let create ?padded key =
    let open struct
      type 'k t = { mutable state : 'k State.t [@atomic] }
    end in
    let t = Portable_common.Padding.copy_as ?padded { state = make key } in
    [%atomic.loc t.state]
  ;;
end

module Await = struct
  module State = struct
    type 'k t = Prim.State.t
  end

  include Mutex_common.Make_await (Prim)

  let make _ = Prim.State.make ()
end
