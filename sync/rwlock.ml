open! Base
open! Import
module Capsule = Capsule.Prim

(** See [Adaptive_backoff.once] *)
let log_scale = 10

module Prim = struct
  module State : sig @@ portable
    (** Implements primitive read-write lock state manipulation logic.

        The state has several main features:

        - a [shared] count,

        - a (very large) threshold [shared] count that indicates that the rwlock is
          permanently shared,

        - a bit indicating that the lock is held [exclusive],

        - a (very large) threshold indicating that the lock is [exclusive] permanently,

        - a bit indicating that there are potentially readers awaiting, and

        - a bit indicating that there are potentially writers awaiting.

        State values are allowed to be momentarily imprecise:

        - The bits that indicate that there are readers / writers awaiting are allowed to
          momentarily say that there are no awaiters when, in fact, there are awaiters and
          also the opposite that there are awaiters when, in fact, there is none.

        - The [shared] count may be non-zero while the [exclusive] bit is set.

        To make this possible, state changes are expressed as incremental updates to
        current state that retain information from the previous state and actively
        contending operations work cooperatively on the state:

        - Both [acquire_shared] and [release_shared] may need to undo their increment or
          decrement, respectively, of the [shared] count.

        - The responsibility to signal awaiters, and update the state to indicate that
          there are no awaiters, is transferred to whichever thread is the last to release
          the mutex while there potentially are awaiters.

        - Awoken awaiters must actively re-establish that some thread might be awaiting to
          read or write. *)

    type t : immediate

    val no_awaiters : t

    (* *)

    val incr_shared : t Awaitable.t @ local -> t
    val decr_shared : t Awaitable.t @ local -> t

    (* *)

    val is_unlocked : t -> bool
    val is_unlocked_and_has_awaiters : t -> bool
    val is_permanently : t -> bool
    val is_shared_permanently : t -> bool
    val is_exclusive : t -> bool
    val is_exclusive_permanently : t -> bool
    val has_awaiters : t -> bool
    val has_readers : t -> bool
    val is_shared_at_most_once_and_has_awaiters : t -> bool

    (* *)

    val and_shared_permanently : t -> t
    val and_exclusive : t -> t
    val and_exclusive_permanently : t -> t
    val and_not_exclusive : t -> t
    val and_readers : t -> t
    val and_no_readers : t -> t
    val and_writers : t -> t
    val and_no_awaiters : t -> t
    val and_incr_shared : t -> t
    val and_decr_shared : t -> t
    val and_downgrade : t -> t
  end = struct
    (* The state looks like this:

       {v
         Bit: [      0     |      1     | 2 to n-7 | n-6 |   n-5  |   n-4     | n-3 |   n-2     | n-1 ]
         Use: [ no_writers ! no_readers |  shared  |     ! shared | exclusive |     ! exclusive |  0  ]
              [       no_awaiters       |  count   |        perm. |           |         perm.   |     ]
       v}

       Above, [n] is the number of bits in an [int] and bit at [n-1] is the sign bit,
       which should normally always be [0].

       The [n-3] bit allows the [exclusive_permanently] bit to be temporarily [0] due to
       an optimistic decrement of the [shared] count by [release_shared].

       The number of bits for [shared] count should be enough that it is (practically)
       impossible to overflow on a 64-bit system even with a silly program that just
       repeatedly calls [acquire_shared]. *)

    type t = int

    let no_writers = 1 lsl 0
    let no_readers = 1 lsl 1
    let no_awaiters = no_readers lor no_writers
    let shared = 1 lsl 2
    let shared_permanently = 1 lsl (Int.num_bits - 5)
    let exclusive = 1 lsl (Int.num_bits - 4)
    let exclusive_permanently = 1 lsl (Int.num_bits - 2)

    (* *)

    let[@inline] incr_shared t = Awaitable.fetch_and_add t shared
    let[@inline] decr_shared t = Awaitable.fetch_and_add t (-shared)

    (* *)

    let[@inline] is_unlocked state = state < shared
    let[@inline] is_unlocked_and_has_awaiters state = state < no_awaiters

    let[@inline] is_permanently state =
      shared_permanently / 2 <= state land lnot exclusive
    ;;

    let[@inline] is_shared_permanently state = shared_permanently / 2 <= state
    let[@inline] is_exclusive_permanently state = exclusive_permanently / 2 <= state
    let[@inline] is_exclusive state = exclusive <= state
    let[@inline] has_awaiters state = state land no_awaiters <> no_awaiters
    let[@inline] has_readers state = state land no_readers = 0

    let[@inline] is_shared_at_most_once_and_has_awaiters state =
      state < shared lor no_awaiters
    ;;

    (* *)

    let[@inline] and_shared_permanently state = state lor shared_permanently
    let[@inline] and_exclusive state = state lor exclusive
    let[@inline] and_exclusive_permanently state = state lor exclusive_permanently
    let[@inline] and_not_exclusive state = state land lnot exclusive
    let[@inline] and_readers state = state land lnot no_readers
    let[@inline] and_no_readers state = state lor no_readers
    let[@inline] and_writers state = state land lnot no_writers
    let[@inline] and_no_awaiters state = state lor no_awaiters
    let[@inline] and_incr_shared state = state + shared
    let[@inline] and_decr_shared state = state - shared
    let[@inline] and_downgrade state = state |> and_not_exclusive |> and_incr_shared
  end

  type t = State.t Awaitable.t

  type ('a, _, 'w) result =
    | Value : ('a, 'a, Await.t) result
    | Or_canceled : ('a, 'a Or_canceled.t, Await.t) result
    | Or_would_block : ('a, 'a Or_would_block.t, unit) result

  let[@inline never] acquire_as (type r w) (w : w) c t (r : (unit, r, w) result) : r =
    let[@inline] completed () : r =
      match r with
      | Value -> ()
      | Or_canceled -> Completed ()
      | Or_would_block -> Acquired ()
    in
    (* This is intentionally optimized optimistically for low contention. *)
    let assumption = State.no_awaiters in
    let after = State.no_awaiters |> State.and_exclusive in
    let before =
      Awaitable.compare_exchange t ~if_phys_equal_to:assumption ~replace_with:after
    in
    if phys_equal before assumption
    then completed ()
    else (
      let[@inline] rec acquire_awaiting (w : w) c t before =
        if State.is_unlocked before
        then (
          let after = before |> State.and_writers |> State.and_exclusive in
          match
            Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after
          with
          | Set_here -> completed ()
          | Compare_failed ->
            Adaptive_backoff.once ~random_key:(Awaitable.random_key t) ~log_scale;
            acquire_awaiting w c t (Awaitable.get t))
        else if State.is_permanently before
        then raise (if State.is_exclusive before then Poisoned else Frozen)
        else (
          let after = before |> State.and_writers in
          if phys_equal before after
             ||
             match
               Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after
             with
             | Set_here -> true
             | Compare_failed -> false
          then (
            match r with
            | Value ->
              (match Awaitable.await w t ~until_phys_unequal_to:after with
               | Signaled -> acquire_awaiting w c t (Awaitable.get t)
               | Terminated -> raise Await.Terminated)
            | Or_canceled ->
              (match Awaitable.await_or_cancel w c t ~until_phys_unequal_to:after with
               | Signaled -> acquire_awaiting w c t (Awaitable.get t)
               | Terminated -> raise Await.Terminated
               | Canceled -> Or_canceled.Canceled)
            | Or_would_block -> Or_would_block.Would_block)
          else (
            Adaptive_backoff.once ~random_key:(Awaitable.random_key t) ~log_scale;
            acquire_awaiting w c t (Awaitable.get t)))
      in
      acquire_awaiting w c t before)
  ;;

  let acquire_or_cancel w c t = acquire_as w c t Or_canceled
  let acquire w t = acquire_as w Cancellation.never t Value
  let try_acquire t = acquire_as () Cancellation.never t Or_would_block

  let[@inline never] release t =
    (* This is intentionally optimized optimistically for low contention. *)
    let assumption = State.no_awaiters |> State.and_exclusive in
    let after = State.no_awaiters in
    let before =
      Awaitable.compare_exchange t ~if_phys_equal_to:assumption ~replace_with:after
    in
    if not (phys_equal before assumption)
    then (
      let[@inline] rec release_contended t before =
        if not (State.is_permanently before)
        then (
          let after = before |> State.and_not_exclusive |> State.and_no_awaiters in
          match
            Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after
          with
          | Set_here ->
            if State.has_awaiters before
            then
              if State.has_readers before
              then Awaitable.broadcast t
              else Awaitable.signal t
          | Compare_failed ->
            Adaptive_backoff.once ~random_key:(Awaitable.random_key t) ~log_scale;
            release_contended t (Awaitable.get t))
      in
      release_contended t before)
  ;;

  let[@inline never] release_and_reraise exn t =
    let bt = Backtrace.Exn.most_recent () in
    release t;
    Exn.raise_with_original_backtrace exn bt
  ;;

  let[@inline never] acquire_shared_as (type r w) (w : w) c t (r : (unit, r, w) result)
    : r
    =
    let[@inline] completed () : r =
      match r with
      | Value -> ()
      | Or_canceled -> Completed ()
      | Or_would_block -> Acquired ()
    in
    let prior = State.incr_shared t in
    if not (State.is_exclusive prior)
    then completed ()
    else (
      let[@inline] rec acquire_shared_awaiting (w : w) c t =
        let before = Awaitable.get t in
        if not (State.is_exclusive before)
        then (
          let after = before |> State.and_incr_shared |> State.and_readers in
          match
            Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after
          with
          | Set_here -> completed ()
          | Compare_failed ->
            Adaptive_backoff.once ~random_key:(Awaitable.random_key t) ~log_scale;
            acquire_shared_awaiting w c t)
        else if State.is_exclusive_permanently before
        then raise Poisoned
        else (
          let after = before |> State.and_readers in
          if phys_equal before after
             ||
             match
               Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after
             with
             | Set_here -> true
             | Compare_failed -> false
          then (
            match r with
            | Value ->
              (match Awaitable.await w t ~until_phys_unequal_to:after with
               | Signaled -> acquire_shared_awaiting w c t
               | Terminated -> raise Await.Terminated)
            | Or_canceled ->
              (match Awaitable.await_or_cancel w c t ~until_phys_unequal_to:after with
               | Signaled -> acquire_shared_awaiting w c t
               | Terminated -> raise Await.Terminated
               | Canceled -> Or_canceled.Canceled)
            | Or_would_block -> Or_would_block.Would_block)
          else (
            Adaptive_backoff.once ~random_key:(Awaitable.random_key t) ~log_scale;
            acquire_shared_awaiting w c t))
      in
      let[@inline] rec acquire_shared_contended w c t =
        let before = Awaitable.get t in
        if not (State.is_exclusive before)
        then completed ()
        else (
          let after = before |> State.and_decr_shared |> State.and_readers in
          match
            Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after
          with
          | Set_here -> acquire_shared_awaiting w c t
          | Compare_failed ->
            Adaptive_backoff.once ~random_key:(Awaitable.random_key t) ~log_scale;
            acquire_shared_contended w c t)
      in
      acquire_shared_contended w c t)
  ;;

  let acquire_shared_or_cancel w c t = acquire_shared_as w c t Or_canceled
  let acquire_shared w t = acquire_shared_as w Cancellation.never t Value
  let try_acquire_shared t = acquire_shared_as () Cancellation.never t Or_would_block

  let[@inline never] release_shared t =
    let prior = State.decr_shared t in
    if State.is_shared_at_most_once_and_has_awaiters prior
    then (
      let[@inline] rec signal_awaiters t =
        let before = Awaitable.get t in
        if State.is_unlocked_and_has_awaiters before
        then (
          match
            Awaitable.compare_and_set
              t
              ~if_phys_equal_to:before
              ~replace_with:State.no_awaiters
          with
          | Set_here ->
            if State.has_readers before then Awaitable.broadcast t else Awaitable.signal t
          | Compare_failed ->
            (* Backoff is probably not worth the trouble here as contending state changes
               potentially change the state such that we no longer need to signal. *)
            signal_awaiters t)
      in
      signal_awaiters t)
    else if State.is_exclusive_permanently prior
    then (
      let[@inline] undo_release_shared t =
        let _ : State.t = State.incr_shared t in
        ()
      in
      undo_release_shared t)
  ;;

  let[@inline never] release_shared_and_reraise exn t =
    let bt = Backtrace.Exn.most_recent () in
    release_shared t;
    Exn.raise_with_original_backtrace exn bt
  ;;

  let[@inline never] rec poison t =
    let before = Awaitable.get t in
    (* Unfortunately we cannot check ownership at this point. *)
    if not (State.is_exclusive_permanently before)
    then (
      let after = State.no_awaiters |> State.and_exclusive_permanently in
      match Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after with
      | Set_here -> Awaitable.broadcast t
      | Compare_failed ->
        (* Backoff is probably not worth the trouble. *)
        poison t)
  ;;

  let[@inline never] poison_and_reraise exn t =
    let bt = Backtrace.Exn.most_recent () in
    poison t;
    Exn.raise_with_original_backtrace exn bt
  ;;

  (** Note that [Prim.freeze] is idempotent and you typically need to
      [Prim.release_shared] the lock after calling [Prim.freeze] to avoid having the
      shared count grow without bound.

      The idea behind having to [Prim.release_shared] even after freezing a lock is that
      this allows [Prim.acquire_shared] and [Prim.release_shared] to be as fast as
      possible, i.e. just a single atomic [fetch_and_add] touching the lock word, in the
      ordinary cases. *)
  let[@inline never] freeze t =
    let before = ref (Awaitable.get t : State.t) in
    assert%debug ((not (State.is_unlocked !before)) && not (State.is_exclusive !before));
    while
      (not (State.is_shared_permanently !before))
      &&
      let after = !before |> State.and_shared_permanently |> State.and_no_awaiters in
      (* This leaves the rwlock as read locked. *)
      match Awaitable.compare_and_set t ~if_phys_equal_to:!before ~replace_with:after with
      | Set_here -> false
      | Compare_failed -> true
    do
      (* Backoff is probably not worth the trouble. *)
      before := Awaitable.get t
    done;
    (* We must wake up any writers waiting to obtain the lock. *)
    if State.has_awaiters !before then Awaitable.broadcast t
  ;;

  let[@inline never] freeze_release_shared_and_reraise exn t =
    let bt = Backtrace.Exn.most_recent () in
    freeze t;
    release_shared t;
    Exn.raise_with_original_backtrace exn bt
  ;;

  let downgrade t =
    (* This is intentionally optimized optimistically for low contention. *)
    let assumption = State.no_awaiters |> State.and_exclusive in
    let after = State.no_awaiters |> State.and_incr_shared in
    let before =
      Awaitable.compare_exchange t ~if_phys_equal_to:assumption ~replace_with:after
    in
    if not (phys_equal before assumption)
    then (
      assert%debug (State.is_exclusive before && not (State.is_permanently before));
      let rec downgrade_contended t before =
        let after = before |> State.and_downgrade |> State.and_no_readers in
        match
          Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after
        with
        | Set_here -> if State.has_readers before then Awaitable.broadcast t
        | Compare_failed ->
          Adaptive_backoff.once ~random_key:(Awaitable.random_key t) ~log_scale;
          downgrade_contended t (Awaitable.get t)
      in
      downgrade_contended t before)
  ;;

  let[@inline] create ?padded () = Awaitable.make ?padded State.no_awaiters
end

module [@inline] Make (C : Lock_common.Capability_with_of_await) = struct
  type 'k t = Prim.t

  let create ?padded _key = Prim.create ?padded ()

  module Shared_guard = struct
    type 'k rwlock = 'k t

    type 'k inner =
      { rwlock : 'k rwlock @@ many
      ; mutable should_freeze : bool [@atomic]
      }

    type 'k t = { inner : 'k inner @@ aliased contended portable } [@@unboxed]

    let freeze_if_locked ({ rwlock; _ } as inner) =
      if Atomic.Loc.get [%atomic.loc inner.should_freeze]
      then (
        Prim.freeze rwlock;
        Prim.release_shared rwlock)
    ;;

    let create : 'k rwlock -> 'k t @ unique =
      fun rwlock ->
      let t = { rwlock; should_freeze = true } in
      Stdlib.Gc.Safe.finalise freeze_if_locked t;
      { inner = t }
    ;;

    let with_key
      :  'k t @ unique -> f:('k Capsule.Key.t -> 'a @ once unique) @ local once
      -> 'a * 'k t @ once unique
      =
      fun t ~f ->
      match f (Capsule.Key.unsafe_mk ()) with
      | res -> res, t
      | exception exn ->
        let bt = Backtrace.Exn.most_recent () in
        Prim.freeze t.inner.rwlock;
        Exn.raise_with_original_backtrace exn bt
    ;;

    let with_password t ~f =
      let { many = a }, t =
        with_key t ~f:(fun key ->
          Capsule.Key.with_password_shared key ~f:(fun password -> { many = f password })
          [@nontail])
      in
      a, t
    ;;

    let access t ~f =
      let { contended = { portable = a } }, t =
        with_key t ~f:(fun key ->
          { contended =
              Capsule.Key.access_shared key ~f:(fun access -> { portable = f access })
          })
      in
      a, t
    ;;

    let release { inner = { rwlock; _ } as inner } =
      Atomic.Loc.set [%atomic.loc inner.should_freeze] false;
      (* Make sure the stack root for [inner] stays alive at least this long, to make sure
         that if its finalizer runs, it sees [should_poison] set to [false]. *)
      let _ : _ = (Sys.opaque_identity [@mode contended]) inner in
      Prim.release_shared rwlock
    ;;

    let freeze : 'k t @ unique -> 'k Capsule.Key.t =
      fun { inner = { rwlock; _ } as inner } ->
      (* Not technically releasing, but setting this value to [false] prevents calling
         [freeze] again in the finalizer, which is moderately more efficient. *)
      Atomic.Loc.set [%atomic.loc inner.should_freeze] false;
      Prim.freeze rwlock;
      Prim.release_shared rwlock;
      Capsule.Key.unsafe_mk ()
    ;;
  end

  module Guard = struct
    type 'k rwlock = 'k t

    type 'k inner =
      { rwlock : 'k rwlock @@ many
      ; mutable should_poison : bool [@atomic]
      }

    type 'k t = { inner : 'k inner @@ aliased contended portable } [@@unboxed]

    let poison_if_locked ({ rwlock; _ } as inner) =
      if Atomic.Loc.get [%atomic.loc inner.should_poison] then Prim.poison rwlock
    ;;

    let create : 'k rwlock -> 'k t @ unique =
      fun rwlock ->
      let t = { rwlock; should_poison = true } in
      Stdlib.Gc.Safe.finalise poison_if_locked t;
      { inner = t }
    ;;

    let with_key
      :  'k t @ unique
      -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ once unique)
         @ local once
      -> 'a * 'k t @ once unique
      =
      fun t ~f ->
      match f (Capsule.Key.unsafe_mk ()) with
      | #(res, _key) -> res, t
      | exception exn -> Prim.poison_and_reraise exn t.inner.rwlock
    ;;

    let with_password
      :  'k t @ unique -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
      -> 'a * 'k t @ unique
      =
      fun t ~f ->
      let { many = a }, t =
        with_key
          t
          ~f:
            (Capsule.Key.with_password ~f:(fun password -> { many = f password })
            [@nontail])
      in
      a, t
    ;;

    let access t ~f =
      let { contended = { portable = a } }, t =
        with_key t ~f:(fun key ->
          let #(a, key) =
            Capsule.Key.access key ~f:(fun access -> { portable = f access })
          in
          #({ contended = a }, key))
      in
      a, t
    ;;

    let release { inner = { rwlock; _ } as inner } =
      Atomic.Loc.set [%atomic.loc inner.should_poison] false;
      (* Make sure the stack root for [inner] stays alive at least this long, to make sure
         that if its finalizer runs, it sees [should_poison] set to [false]. *)
      let _ : _ = (Sys.opaque_identity [@mode contended]) inner in
      Prim.release rwlock
    ;;

    let poison { inner = { rwlock; _ } as inner } =
      (* Not technically releasing, but setting this value to [false] prevents calling
         [poison] again in the finalizer, which is moderately more efficient. *)
      Atomic.Loc.set [%atomic.loc inner.should_poison] false;
      Prim.poison rwlock;
      Capsule.Key.unsafe_mk ()
    ;;

    let downgrade { inner = { rwlock; _ } as inner } : _ Shared_guard.t =
      Prim.downgrade rwlock;
      Atomic.Loc.set [%atomic.loc inner.should_poison] false;
      (* Make sure the stack root for [inner] stays alive at least this long, to make sure
         that if its finalizer runs, it sees [should_poison] set to [false]. *)
      let _ : _ = (Sys.opaque_identity [@mode contended]) inner in
      Shared_guard.create rwlock
    ;;
  end

  module Unsafe = struct
    (* SAFETY:

       Since these functions unlock the mutex on uncaught exceptions rather than
       poisoning, they are unsound: it's possible to stash the provided key in a raised
       exception, which leaks the key. We define them internally to implement functions
       like [with_access] and [with_password], but intentionally don't expose them in the
       interface.
    *)

    let[@inline] with_key
      : ('a : value_or_null) 'k.
      C.t @ local
      -> 'k t @ local
      -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ once unique)
         @ local once
      -> 'a @ once unique
      =
      fun w t ~f ->
      Prim.acquire (C.unsafe_to_await w) t;
      match f (Capsule.Key.unsafe_mk ()) with
      | #(res, _key) ->
        Prim.release t;
        res
      | exception exn -> Prim.release_and_reraise exn t
    ;;

    let[@inline] try_with_key
      : ('a : value_or_null) 'k.
      'k t @ local
      -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ once unique)
         @ local once
      -> 'a Or_would_block.t @ once unique
      =
      fun t ~f ->
      match Prim.try_acquire t with
      | Would_block -> Would_block
      | Acquired () ->
        (match f (Capsule.Key.unsafe_mk ()) with
         | #(res, _key) ->
           Prim.release t;
           Acquired res
         | exception exn -> Prim.release_and_reraise exn t)
    ;;

    let[@inline] with_key_or_cancel
      : ('a : value_or_null) 'k.
      C.t @ local
      -> Cancellation.t @ local
      -> 'k t @ local
      -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ once unique)
         @ local once
      -> 'a Or_canceled.t @ once unique
      =
      fun w c t ~f ->
      match Prim.acquire_or_cancel (C.unsafe_to_await w) c t with
      | Canceled -> Canceled
      | Completed () ->
        (match f (Capsule.Key.unsafe_mk ()) with
         | #(res, _key) ->
           Prim.release t;
           Completed res
         | exception exn -> Prim.release_and_reraise exn t)
    ;;

    let[@inline] with_key_shared
      : ('a : value_or_null) 'k.
      C.t @ local
      -> 'k t @ local
      -> f:('k Capsule.Key.t -> 'a @ once unique) @ local once
      -> 'a @ once unique
      =
      fun w t ~f ->
      Prim.acquire_shared (C.unsafe_to_await w) t;
      match f (Capsule.Key.unsafe_mk ()) with
      | res ->
        Prim.release_shared t;
        res
      | exception exn -> Prim.release_shared_and_reraise exn t
    ;;

    let[@inline] with_key_shared_freezing
      : ('a : value_or_null) 'k.
      C.t @ local
      -> 'k t @ local
      -> f:('k Capsule.Key.t -> 'a @ once unique) @ local once
      -> 'a @ once unique
      =
      fun w t ~f ->
      Prim.acquire_shared (C.unsafe_to_await w) t;
      match f (Capsule.Key.unsafe_mk ()) with
      | res ->
        Prim.release_shared t;
        res
      | exception exn -> Prim.freeze_release_shared_and_reraise exn t
    ;;

    let[@inline] try_with_key_shared
      : ('a : value_or_null) 'k.
      'k t @ local
      -> f:('k Capsule.Key.t -> 'a @ once unique) @ local once
      -> 'a Or_would_block.t @ once unique
      =
      fun t ~f ->
      match Prim.try_acquire_shared t with
      | Would_block -> Would_block
      | Acquired () ->
        (match f (Capsule.Key.unsafe_mk ()) with
         | res ->
           Prim.release_shared t;
           Acquired res
         | exception exn -> Prim.release_shared_and_reraise exn t)
    ;;

    let[@inline] try_with_key_shared_freezing
      :  'k t @ local -> f:('k Capsule.Key.t -> 'a @ once unique) @ local once
      -> 'a Or_would_block.t @ once unique
      =
      fun t ~f ->
      match Prim.try_acquire_shared t with
      | Would_block -> Would_block
      | Acquired () ->
        (match f (Capsule.Key.unsafe_mk ()) with
         | res ->
           Prim.release_shared t;
           Acquired res
         | exception exn -> Prim.freeze_release_shared_and_reraise exn t)
    ;;

    let[@inline] with_key_shared_or_cancel
      : ('a : value_or_null) 'k.
      C.t @ local
      -> Cancellation.t @ local
      -> 'k t @ local
      -> f:('k Capsule.Key.t -> 'a @ once unique) @ local once
      -> 'a Or_canceled.t @ once unique
      =
      fun w c t ~f ->
      match Prim.acquire_shared_or_cancel (C.unsafe_to_await w) c t with
      | Canceled -> Canceled
      | Completed () ->
        (match f (Capsule.Key.unsafe_mk ()) with
         | res ->
           Prim.release_shared t;
           Completed res
         | exception exn -> Prim.release_shared_and_reraise exn t)
    ;;

    let[@inline] with_key_shared_or_cancel_freezing
      :  C.t @ local -> Cancellation.t @ local -> 'k t @ local
      -> f:('k Capsule.Key.t -> 'a @ once unique) @ local once
      -> 'a Or_canceled.t @ once unique
      =
      fun w c t ~f ->
      match Prim.acquire_shared_or_cancel (C.unsafe_to_await w) c t with
      | Canceled -> Canceled
      | Completed () ->
        (match f (Capsule.Key.unsafe_mk ()) with
         | res ->
           Prim.release_shared t;
           Completed res
         | exception exn -> Prim.freeze_release_shared_and_reraise exn t)
    ;;
  end

  [%%template
  [@@@mode.default l = (global, local)]

  let[@inline] with_key_poisoning
    : ('a : value_or_null) 'k.
    C.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
       @ local once
    -> 'a @ l once unique
    =
    fun w t ~f ->
    (Prim.acquire (C.unsafe_to_await w) t;
     match f (Capsule.Key.unsafe_mk ()) with
     | #(res, _key) ->
       Prim.release t;
       res
     | exception exn -> Prim.poison_and_reraise exn t)
    [@exclave_if_local l ~reasons:[ May_return_local ]]
  ;;

  let[@inline] with_key_or_cancel_poisoning
    : ('a : value_or_null) 'k.
    C.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
       @ local once
    -> 'a Or_canceled.t @ l once unique
    =
    fun w c t ~f ->
    match[@exclave_if_local l ~reasons:[ May_return_local ]]
      Prim.acquire_or_cancel (C.unsafe_to_await w) c t
    with
    | Canceled -> Canceled
    | Completed () ->
      (match f (Capsule.Key.unsafe_mk ()) with
       | #(res, _key) ->
         Prim.release t;
         Completed res
       | exception exn -> Prim.poison_and_reraise exn t)
  ;;

  let[@inline] try_with_key_poisoning
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
       @ local once
    -> 'a Or_would_block.t @ l once unique
    =
    fun t ~f ->
    match[@exclave_if_local l ~reasons:[ May_return_local ]] Prim.try_acquire t with
    | Would_block -> Would_block
    | Acquired () ->
      (match f (Capsule.Key.unsafe_mk ()) with
       | #(res, _key) ->
         Prim.release t;
         Acquired res
       | exception exn -> Prim.poison_and_reraise exn t)
  ;;]

  let acquire w t =
    Prim.acquire w t;
    Guard.create t
  ;;

  let try_acquire t : _ Or_would_block.t =
    match Prim.try_acquire t with
    | Would_block -> Would_block
    | Acquired () -> Acquired (Guard.create t)
  ;;

  let acquire_or_cancel w c t : _ Or_canceled.t =
    match Prim.acquire_or_cancel w c t with
    | Canceled -> Canceled
    | Completed () -> Completed (Guard.create t)
  ;;

  let acquire_shared w t =
    Prim.acquire_shared w t;
    Shared_guard.create t
  ;;

  let try_acquire_shared t : _ Or_would_block.t =
    match Prim.try_acquire_shared t with
    | Would_block -> Would_block
    | Acquired () -> Acquired (Shared_guard.create t)
  ;;

  let acquire_shared_or_cancel w c t : _ Or_canceled.t =
    match Prim.acquire_shared_or_cancel w c t with
    | Canceled -> Canceled
    | Completed () -> Completed (Shared_guard.create t)
  ;;

  let with_access w t ~f =
    (Unsafe.with_key w t ~f:(fun key ->
       let #(a, key) =
         Capsule.Key.access key ~f:(fun access -> { portable = f access })
       in
       #({ contended = a }, key)))
      .contended
      .portable
  ;;

  let with_access_poisoning w t ~f =
    (with_key_poisoning w t ~f:(fun key ->
       let #(a, key) =
         Capsule.Key.access key ~f:(fun access -> { portable = f access })
       in
       #({ contended = a }, key)))
      .contended
      .portable
  ;;

  let try_with_access t ~f : _ Or_would_block.t =
    match
      Unsafe.try_with_key t ~f:(fun key ->
        let #(a, key) =
          Capsule.Key.access key ~f:(fun access -> { portable = f access })
        in
        #({ contended = a }, key))
    with
    | Would_block -> Would_block
    | Acquired { contended = { portable = a } } -> Acquired a
  ;;

  let try_with_access_poisoning t ~f : _ Or_would_block.t =
    match
      try_with_key_poisoning t ~f:(fun key ->
        let #(a, key) =
          Capsule.Key.access key ~f:(fun access -> { portable = f access })
        in
        #({ contended = a }, key))
    with
    | Would_block -> Would_block
    | Acquired { contended = { portable = a } } -> Acquired a
  ;;

  let with_access_or_cancel w c t ~f : _ Or_canceled.t =
    match
      Unsafe.with_key_or_cancel w c t ~f:(fun key ->
        let #(a, key) =
          Capsule.Key.access key ~f:(fun access -> { portable = f access })
        in
        #({ contended = a }, key))
    with
    | Canceled -> Canceled
    | Completed { contended = { portable = a } } -> Completed a
  ;;

  let with_access_or_cancel_poisoning w c t ~f : _ Or_canceled.t =
    match
      with_key_or_cancel_poisoning w c t ~f:(fun key ->
        let #(a, key) =
          Capsule.Key.access key ~f:(fun access -> { portable = f access })
        in
        #({ contended = a }, key))
    with
    | Canceled -> Canceled
    | Completed { contended = { portable = a } } -> Completed a
  ;;

  let with_access_shared w t ~f =
    (Unsafe.with_key_shared w t ~f:(fun key ->
       { contended =
           Capsule.Key.access_shared key ~f:(fun access -> { portable = f access })
       }))
      .contended
      .portable
  ;;

  let with_access_shared_freezing w t ~f =
    (Unsafe.with_key_shared_freezing w t ~f:(fun key ->
       { contended =
           Capsule.Key.access_shared key ~f:(fun access -> { portable = f access })
       }))
      .contended
      .portable
  ;;

  let try_with_access_shared t ~f : _ Or_would_block.t =
    match
      Unsafe.try_with_key_shared t ~f:(fun key ->
        { contended =
            Capsule.Key.access_shared key ~f:(fun access -> { portable = f access })
        })
    with
    | Would_block -> Would_block
    | Acquired { contended = { portable = a } } -> Acquired a
  ;;

  let try_with_access_shared_freezing t ~f : _ Or_would_block.t =
    match
      Unsafe.try_with_key_shared_freezing t ~f:(fun key ->
        { contended =
            Capsule.Key.access_shared key ~f:(fun access -> { portable = f access })
        })
    with
    | Would_block -> Would_block
    | Acquired { contended = { portable = a } } -> Acquired a
  ;;

  let with_access_shared_or_cancel w c t ~f : _ Or_canceled.t =
    match
      Unsafe.with_key_shared_or_cancel w c t ~f:(fun key ->
        { contended =
            Capsule.Key.access_shared key ~f:(fun access -> { portable = f access })
        })
    with
    | Canceled -> Canceled
    | Completed { contended = { portable = a } } -> Completed a
  ;;

  let with_access_shared_or_cancel_freezing w c t ~f : _ Or_canceled.t =
    match
      Unsafe.with_key_shared_or_cancel_freezing w c t ~f:(fun key ->
        { contended =
            Capsule.Key.access_shared key ~f:(fun access -> { portable = f access })
        })
    with
    | Canceled -> Canceled
    | Completed { contended = { portable = a } } -> Completed a
  ;;

  let with_password w t ~f =
    (Unsafe.with_key w t ~f:(fun key ->
       Capsule.Key.with_password key ~f:(fun password -> { many = f password }) [@nontail]))
      .many
  ;;

  let with_password_poisoning w t ~f =
    (with_key_poisoning w t ~f:(fun key ->
       Capsule.Key.with_password key ~f:(fun password -> { many = f password }) [@nontail]))
      .many
  ;;

  let try_with_password t ~f : _ Or_would_block.t =
    match
      Unsafe.try_with_key t ~f:(fun key ->
        Capsule.Key.with_password key ~f:(fun password -> { many = f password })
        [@nontail])
    with
    | Would_block -> Would_block
    | Acquired { many = a } -> Acquired a
  ;;

  let try_with_password_poisoning t ~f : _ Or_would_block.t =
    match
      try_with_key_poisoning t ~f:(fun key ->
        Capsule.Key.with_password key ~f:(fun password -> { many = f password })
        [@nontail])
    with
    | Would_block -> Would_block
    | Acquired { many = a } -> Acquired a
  ;;

  let with_password_or_cancel w c t ~f : _ Or_canceled.t =
    match
      Unsafe.with_key_or_cancel w c t ~f:(fun key ->
        Capsule.Key.with_password key ~f:(fun password -> { many = f password })
        [@nontail])
    with
    | Canceled -> Canceled
    | Completed { many = a } -> Completed a
  ;;

  let with_password_or_cancel_poisoning w c t ~f : _ Or_canceled.t =
    match
      with_key_or_cancel_poisoning w c t ~f:(fun key ->
        Capsule.Key.with_password key ~f:(fun password -> { many = f password })
        [@nontail])
    with
    | Canceled -> Canceled
    | Completed { many = a } -> Completed a
  ;;

  let with_password_shared w t ~f =
    (Unsafe.with_key_shared w t ~f:(fun key ->
       Capsule.Key.with_password_shared key ~f:(fun password -> { many = f password })
       [@nontail]))
      .many
  ;;

  let with_password_shared_freezing w t ~f =
    (Unsafe.with_key_shared_freezing w t ~f:(fun key ->
       Capsule.Key.with_password_shared key ~f:(fun password -> { many = f password })
       [@nontail]))
      .many
  ;;

  let try_with_password_shared t ~f : _ Or_would_block.t =
    match
      Unsafe.try_with_key_shared t ~f:(fun key ->
        Capsule.Key.with_password_shared key ~f:(fun password -> { many = f password })
        [@nontail])
    with
    | Would_block -> Would_block
    | Acquired { many = a } -> Acquired a
  ;;

  let try_with_password_shared_freezing t ~f : _ Or_would_block.t =
    match
      Unsafe.try_with_key_shared_freezing t ~f:(fun key ->
        Capsule.Key.with_password_shared key ~f:(fun password -> { many = f password })
        [@nontail])
    with
    | Would_block -> Would_block
    | Acquired { many = a } -> Acquired a
  ;;

  let with_password_shared_or_cancel w c t ~f : _ Or_canceled.t =
    match
      Unsafe.with_key_shared_or_cancel w c t ~f:(fun key ->
        Capsule.Key.with_password_shared key ~f:(fun password -> { many = f password })
        [@nontail])
    with
    | Canceled -> Canceled
    | Completed { many = a } -> Completed a
  ;;

  let with_password_shared_or_cancel_freezing w c t ~f : _ Or_canceled.t =
    match
      Unsafe.with_key_shared_or_cancel_freezing w c t ~f:(fun key ->
        Capsule.Key.with_password_shared key ~f:(fun password -> { many = f password })
        [@nontail])
    with
    | Canceled -> Canceled
    | Completed { many = a } -> Completed a
  ;;

  let is_poisoned t = Prim.State.is_exclusive_permanently (Awaitable.get t)
  let is_frozen t = Prim.State.is_shared_permanently (Awaitable.get t)

  let poison t key =
    Prim.poison t;
    key
  ;;

  module Condition = Condition.Make (struct
      type nonrec 'k t = 'k t

      let unsafe_acquire w t = Prim.acquire w t
      let unsafe_release t = Prim.release t
    end)

  [%%template
  [@@@mode.default l = (global, local)]

  let[@inline] with_key_and_condition_wait_poisoning
    : type (a : value_or_null) k.
      Await.t @ local
      -> k t @ local
      -> f:
           (k Condition.Wait.t @ local
            -> k Capsule.Key.t @ unique
            -> #(a * k Capsule.Key.t) @ l once unique)
         @ local once
      -> a @ l once unique
    =
    fun w t ~f ->
    (Prim.acquire w t;
     (Condition.with_wait [@mode l])
       w
       t
       (Capsule.Key.unsafe_mk () : k Capsule.Key.t)
       (fun cw key ->
         match[@exclave_if_local l ~reasons:[ May_return_local ]] f cw key with
         | #(res, _key) ->
           (* If the caller has been able to give a key back to us, the lock must be held *)
           assert%debug (Condition.lock_is_held cw);
           Prim.release t;
           res
         | exception exn ->
           if Condition.lock_is_held cw
           then Prim.poison_and_reraise exn t
           else (
             let bt = Backtrace.Exn.most_recent () in
             Exn.raise_with_original_backtrace exn bt)) [@nontail])
    [@exclave_if_local l ~reasons:[ May_return_local ]]
  ;;

  let with_key_and_condition_wait_or_cancel_poisoning
    : type (a : value_or_null) k.
      Await.t @ local
      -> Cancellation.t @ local
      -> k t @ local
      -> f:
           (k Condition.Wait.t @ local
            -> k Capsule.Key.t @ unique
            -> #(a * k Capsule.Key.t) @ l once unique)
         @ local once
      -> a Or_canceled.t @ l once unique
    =
    fun w c t ~f ->
    match[@exclave_if_local l ~reasons:[ May_return_local ]]
      Prim.acquire_or_cancel w c t
    with
    | Canceled -> Canceled
    | Completed () ->
      (Condition.with_wait [@mode l])
        w
        t
        (Capsule.Key.unsafe_mk () : k Capsule.Key.t)
        (fun cw key ->
          match[@exclave_if_local l ~reasons:[ May_return_local ]] f cw key with
          | #(res, _key) ->
            (* If the caller has been able to give a key back to us, the lock must be held *)
            assert%debug (Condition.lock_is_held cw);
            Prim.release t;
            Or_canceled.Completed res
          | exception exn ->
            if Condition.lock_is_held cw
            then Prim.poison_and_reraise exn t
            else (
              let bt = Backtrace.Exn.most_recent () in
              Exn.raise_with_original_backtrace exn bt)) [@nontail]
  ;;]

  module For_testing = struct
    let is_exclusive t = Prim.State.is_exclusive (Awaitable.get t)

    let is_shared t =
      let state = Awaitable.get t in
      (not (Prim.State.is_unlocked state)) && not (Prim.State.is_exclusive state)
    ;;

    include Awaitable.For_testing
  end
end

module Sync = struct
  include Make (Lock_common.Sync)

  let[@inline] with_access s t ~f =
    with_access s t ~f:(fun access -> f s access [@nontail]) [@nontail]
  ;;

  let[@inline] with_access_poisoning s t ~f =
    with_access_poisoning s t ~f:(fun access -> f s access [@nontail]) [@nontail]
  ;;

  let[@inline] with_access_or_cancel s c t ~f =
    with_access_or_cancel s c t ~f:(fun access -> f s access [@nontail]) [@nontail]
  ;;

  let[@inline] with_access_or_cancel_poisoning s c t ~f =
    with_access_or_cancel_poisoning s c t ~f:(fun access -> f s access [@nontail])
    [@nontail]
  ;;

  let[@inline] with_access_shared s t ~f =
    with_access_shared s t ~f:(fun access -> f s access [@nontail]) [@nontail]
  ;;

  let[@inline] with_access_shared_freezing s t ~f =
    with_access_shared_freezing s t ~f:(fun access -> f s access [@nontail]) [@nontail]
  ;;

  let[@inline] with_access_shared_or_cancel s c t ~f =
    with_access_shared_or_cancel s c t ~f:(fun access -> f s access [@nontail]) [@nontail]
  ;;

  let[@inline] with_access_shared_or_cancel_freezing s c t ~f =
    with_access_shared_or_cancel_freezing s c t ~f:(fun access -> f s access [@nontail])
    [@nontail]
  ;;

  let[@inline] with_password s t ~f =
    with_password s t ~f:(fun password -> f s password [@nontail]) [@nontail]
  ;;

  let[@inline] with_password_poisoning s t ~f =
    with_password_poisoning s t ~f:(fun password -> f s password [@nontail]) [@nontail]
  ;;

  let[@inline] with_password_or_cancel s c t ~f =
    with_password_or_cancel s c t ~f:(fun password -> f s password [@nontail]) [@nontail]
  ;;

  let[@inline] with_password_or_cancel_poisoning s c t ~f =
    with_password_or_cancel_poisoning s c t ~f:(fun password -> f s password [@nontail])
    [@nontail]
  ;;

  let[@inline] with_password_shared s t ~f =
    with_password_shared s t ~f:(fun password -> f s password [@nontail]) [@nontail]
  ;;

  let[@inline] with_password_shared_freezing s t ~f =
    with_password_shared_freezing s t ~f:(fun password -> f s password [@nontail])
    [@nontail]
  ;;

  let[@inline] with_password_shared_or_cancel s c t ~f =
    with_password_shared_or_cancel s c t ~f:(fun password -> f s password [@nontail])
    [@nontail]
  ;;

  let[@inline] with_password_shared_or_cancel_freezing s c t ~f =
    with_password_shared_or_cancel_freezing s c t ~f:(fun password ->
      f s password [@nontail])
    [@nontail]
  ;;

  [%%template
  [@@@mode.default l = (global, local)]

  let[@inline] with_key_poisoning s t ~f =
    (with_key_poisoning [@mode l]) s t ~f:(fun key ->
      f s key [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]])
    [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]]
  ;;

  let[@inline] try_with_key_poisoning t ~f =
    (try_with_key_poisoning [@mode l]) t ~f:(fun key ->
      f key [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]])
    [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]]
  ;;

  let[@inline] with_key_or_cancel_poisoning s c t ~f =
    (with_key_or_cancel_poisoning [@mode l]) s c t ~f:(fun key ->
      f s key [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]])
    [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]]
  ;;

  let[@inline] with_key_and_condition_wait_poisoning w t ~f =
    (with_key_and_condition_wait_poisoning [@mode l]) w t ~f:(fun cw key ->
      f
        (Await.sync w)
        cw
        key [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]])
    [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]]
  ;;

  let[@inline] with_key_and_condition_wait_or_cancel_poisoning w c t ~f =
    (with_key_and_condition_wait_or_cancel_poisoning [@mode l]) w c t ~f:(fun cw key ->
      f
        (Await.sync w)
        cw
        key [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]])
    [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]]
  ;;]
end

module Await = Make (Lock_common.Await)
