module type Prim = sig @@ portable
  open Import

  type t : value mod contended forkable non_float portable unyielding

  val create : ?padded:bool @ local -> unit @ local -> t

  (* *)

  val acquire : Await.t @ local -> t @ local -> unit

  val acquire_or_cancel
    :  Await.t @ local
    -> Cancellation.t @ local
    -> t @ local
    -> unit Or_canceled.t

  val try_acquire : t @ local -> bool

  (* *)

  val release : t @ local -> unit
  val release_and_reraise : ('r : value_or_null). exn -> t @ local -> 'r @ portable unique

  (* *)

  val poison : t @ local -> unit
  val poison_and_reraise : ('r : value_or_null). exn -> t @ local -> 'r @ portable unique

  (* *)

  val is_poisoned : t @ local -> bool
  val is_locked : t @ local -> bool

  module For_testing : sig
    val is_exclusive : t @ local -> bool
    val length : t @ local -> int
  end
end

module type Sync = sig @@ portable
  open Await_kernel
  module Capsule := Capsule.Prim

  (** Mutexes that can only be held for a bounded period of time *)

  (** [k t] is the type of a mutex protecting the contents of the [k] capsule which can
      only be held for a bounded period of time. *)
  type 'k t : value mod contended forkable non_float portable unyielding

  (** {1 Creating a mutex} *)

  (** [create k] creates a new mutex for the capsule ['k] associated with [k], consuming
      the key itself.

      The optional [padded] argument specifies whether to pad the data structure to avoid
      false sharing. See {!Atomic.make} for a longer explanation. *)
  val create : ?padded:bool @ local -> 'k Capsule.Key.t @ unique -> 'k t

  (** {1 Executing critical sections} *)

  (** {2 With uncontended [Access]} *)

  [%%template:
  [@@@mode.default l = (local, global)]

  (** [with_access s t ~f] acquires [t], runs [f] within the associated capsule, then
      releases [t]. If [t] is locked then uses [s] to wait until it is unlocked. The
      [Sync.t] is passed to [f] to allow further use of synchronization primitives.

      @raise Poisoned if [t] cannot be acquired because it is poisoned. *)
  val with_access
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> 'k t @ local
    -> f:(Sync.t @ local -> 'k Capsule.Access.t -> 'a @ contended l once portable unique)
       @ local once portable unyielding
    -> 'a @ contended l once portable unique

  (** [try_with_access t ~f] is like {!with_access}, but returns [Would_block] if [t] is
      currently locked. *)
  val try_with_access
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Access.t -> 'a @ contended l once portable unique)
       @ local once portable unyielding
    -> 'a Or_would_block.t @ contended l once portable unique

  (** [with_access_poisoning s t ~f] acquires [t], runs [f] within the associated capsule,
      then releases [t]. If [t] is locked then uses [s] to wait until it is unlocked. The
      [Sync.t] is passed to [f] to allow further use of synchronization primitives.

      Poisons [t] if [f] raises an uncaught exception.

      @raise Poisoned if [t] cannot be acquired because it is poisoned. *)
  val with_access_poisoning
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> 'k t @ local
    -> f:(Sync.t @ local -> 'k Capsule.Access.t -> 'a @ contended l once portable unique)
       @ local once portable unyielding
    -> 'a @ contended l once portable unique

  (** [try_with_access_poisoning t ~f] is like {!with_access_poisoning}, but returns
      [Would_block] if [t] is currently locked. *)
  val try_with_access_poisoning
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Access.t -> 'a @ contended l once portable unique)
       @ local once portable unyielding
    -> 'a Or_would_block.t @ contended l once portable unique

  (** [with_access_or_cancel s c t f] is [Completed (with_access s t f)] if [c] is not
      canceled, otherwise it is [Canceled]. *)
  val with_access_or_cancel
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:(Sync.t @ local -> 'k Capsule.Access.t -> 'a @ contended l once portable unique)
       @ local once portable unyielding
    -> 'a Or_canceled.t @ contended l once portable unique

  (** [with_access_or_cancel_poisoning s c t f] is
      [Completed (with_access_poisoning s t f)] if [c] is not canceled, otherwise it is
      [Canceled].

      Poisons [t] if [f] raises an uncaught exception. *)
  val with_access_or_cancel_poisoning
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:(Sync.t @ local -> 'k Capsule.Access.t -> 'a @ contended l once portable unique)
       @ local once portable unyielding
    -> 'a Or_canceled.t @ contended l once portable unique]

  (** {2 With a local [Password]} *)

  (** [with_password s t f] acquires [t], runs [f] with permission to access the
      associated capsule, then releases [t]. If [t] is locked then uses [s] to wait until
      it is unlocked. The [Sync.t] is passed to [f] to allow further use of
      synchronization primitives.

      @raise Poisoned if [t] cannot be acquired because it is poisoned. *)
  val with_password
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> 'k t @ local
    -> f:(Sync.t @ local -> 'k Capsule.Password.t @ local -> 'a @ unique)
       @ local once unyielding
    -> 'a @ unique

  (** [try_with_password t ~f] is like {!with_password}, but returns [Would_block] if [t]
      is currently locked. *)
  val try_with_password
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once unyielding
    -> 'a Or_would_block.t @ unique

  (** [with_password_poisoning s t f] acquires [t], runs [f] with permission to access the
      associated capsule, then releases [t]. If [t] is locked then uses [s] to wait until
      it is unlocked. The [Sync.t] is passed to [f] to allow further use of
      synchronization primitives.

      Poisons [t] if [f] raises an exception.

      @raise Poisoned if [t] cannot be acquired because it is poisoned. *)
  val with_password_poisoning
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> 'k t @ local
    -> f:(Sync.t @ local -> 'k Capsule.Password.t @ local -> 'a @ unique)
       @ local once unyielding
    -> 'a @ unique

  (** [try_with_password_poisoning t ~f] is like {!with_password_poisoning}, but returns
      [Would_block] if [t] is currently locked. *)
  val try_with_password_poisoning
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once unyielding
    -> 'a Or_would_block.t @ unique

  (** [with_password_or_cancel s c t f] is [Completed (with_password s t f)] if [c] is not
      canceled, otherwise it is [Canceled]. *)
  val with_password_or_cancel
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:(Sync.t @ local -> 'k Capsule.Password.t @ local -> 'a @ unique)
       @ local once unyielding
    -> 'a Or_canceled.t @ unique

  (** [with_password_or_cancel_poisoning s c t f] is
      [Completed (with_password_poisoning s t f)] if [c] is not canceled, otherwise it is
      [Canceled].

      Poisons [t] if [f] raises an exception. *)
  val with_password_or_cancel_poisoning
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:(Sync.t @ local -> 'k Capsule.Password.t @ local -> 'a @ unique)
       @ local once unyielding
    -> 'a Or_canceled.t @ unique

  (** {2 With a unique [Key]} *)

  [%%template:
  [@@@mode.default l = (global, local)]

  (** [with_key_poisoning s t ~f] locks [t] and runs [f], providing it a key for ['k]
      uniquely. If the function raises without returning the key back, the mutex is
      poisoned. If [t] is locked, then [with_key_poisoning] uses [s] to wait until it is
      unlocked. The [Sync.t] is passed to [f] to allow further use of synchronization
      primitives.

      There is no non-poisoning version of this function, because you must return the key
      back (to avoid leaking it via an exception).

      @raise Poisoned if [t] cannot be acquired because it is poisoned. *)
  val with_key_poisoning
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> 'k t @ local
    -> f:
         (Sync.t @ local
          -> 'k Capsule.Key.t @ unique
          -> #('a * 'k Capsule.Key.t) @ l once unique)
       @ local once unyielding
    -> 'a @ l once unique

  (** [try_with_key t ~f] is like {!with_key}, but returns [Would_block] if [t] is
      currently locked. *)
  val try_with_key_poisoning
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
       @ local once unyielding
    -> 'a Or_would_block.t @ l once unique

  (** [with_key_or_cancel_poisoning s c t ~f] is [Completed (with_key_poisoning s t ~f)]
      if [c] is not canceled, otherwise it is [Canceled]. *)
  val with_key_or_cancel_poisoning
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:
         (Sync.t @ local
          -> 'k Capsule.Key.t @ unique
          -> #('a * 'k Capsule.Key.t) @ l once unique)
       @ local once unyielding
    -> 'a Or_canceled.t @ l once unique]

  (** {3 And waiting for a [Condition]} *)

  module Condition : Condition.S with type 'k lock := 'k t (** @open *)

  [%%template:
  [@@@mode.default l = (global, local)]

  (** [with_key_and_condition_wait_poisoning w t ~f] locks [t] and runs [f], providing it
      with a key for ['k] uniquely and a ['k Condition.Wait.t] which provides the ability
      to temporarily unlock [t] and wait on an associated ['k Condition.t]. Since waiting
      on a condition variable can delay for an unbounded period of time, this requires an
      [Await.t], even for a [Sync] mutex, but only passes a [Sync.t] to the callback. To
      recover the original [Await.t] during the execution of [f], you can use
      {!Condition.Wait.release_temporarily}.

      The lock is only poisoned if [f] raises {i while the lock held}; if waiting on a
      condition variable is terminated or if the callback passed to
      {!Condition.Wait.release_temporarily} raises, the lock will not be poisoned.

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Terminated if [w] is terminated before the mutex is acquired. *)
  val with_key_and_condition_wait_poisoning
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> 'k t @ local
    -> f:
         (Sync.t @ local
          -> 'k Condition.Wait.t @ local
          -> 'k Capsule.Key.t @ unique
          -> #('a * 'k Capsule.Key.t) @ l once unique)
       @ local once unyielding
    -> 'a @ l once unique

  (** [with_key_and_condition_wait_or_cancel_poisoining w c t ~f] is
      [Completed (with_key_and_condition_wait_poisoining w t ~f)] if [c] is not canceled,
      otherwise it is [Canceled]. *)
  val with_key_and_condition_wait_or_cancel_poisoning
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:
         (Sync.t @ local
          -> 'k Condition.Wait.t @ local
          -> 'k Capsule.Key.t @ unique
          -> #('a * 'k Capsule.Key.t) @ l once unique)
       @ local once unyielding
    -> 'a Or_canceled.t @ l once unique]

  (** {3 And releasing temporarily} *)

  (** [release_temporarily s t k ~f] releases [t], runs [f] without the mutex, reacquires
      [t] and returns the result of running [f] along with the [k].

      To obtain a key to pass to [release_temporarily], use {!with_key} or similar
      functions.

      @raise Poisoned if [t] cannot be reacquired because it is poisoned. *)
  val release_temporarily
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> 'k t @ local
    -> 'k Capsule.Key.t @ unique
    -> f:(unit -> 'a @ unique) @ local once
    -> #('a * 'k Capsule.Key.t) @ unique

  (** [release_temporarily_or_cancel s c t k ~f] is
      [Completed (release_temporarily s t k ~f)] if [c] is not canceled, otherwise it is
      [Canceled]. *)
  val release_temporarily_or_cancel
    : ('a : value_or_null).
    Sync.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> 'k Capsule.Key.t @ unique
    -> f:(unit -> 'a @ unique) @ local once unyielding
    -> (#('a * 'k Capsule.Key.t) Or_canceled.t[@kind value_or_null & void]) @ unique

  (** {3 And poisoning indefinitely} *)

  (** [poison t key] poisons the mutex associated with the [key]. *)
  val poison : 'k t @ local -> 'k Capsule.Key.t @ unique -> 'k Capsule.Key.t @ unique

  (** {2 Indefinitely through poisoning} *)

  (** [acquire_and_poison s t] acquires [t] and then immediately poisons it, returning the
      key to the protected capsule.

      @raise Poisoned if [t] cannot be acquired because it is poisoned. *)
  val acquire_and_poison : Sync.t @ local -> 'k t @ local -> 'k Capsule.Key.t @ unique

  (** [acquire_and_poison_or_cancel s c t] is [Completed (acquire_and_poison s t)] if [c]
      is not canceled, otherwise it is [Canceled]. *)
  val acquire_and_poison_or_cancel
    :  Sync.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> ('k Capsule.Key.t Or_canceled.t[@kind void]) @ unique

  (** {1 Poisoning as a signal} *)

  (** [is_poisoned t] determines whether the mutex [t] is poisoned. *)
  val is_poisoned : 'k t @ local -> bool

  (** [is_locked t] determines whether the mutex [t] is locked. *)
  val is_locked : 'k t @ local -> bool

  (** [poison_unacquired t] poisons [t] without acquiring it. Does nothing if [t] is
      already poisoned.

      Note that this can render the capsule associated with [t] innaccessible, since it
      potentially destroys the associated key. *)
  val poison_unacquired : 'k t @ local -> unit

  (**/**)

  module For_testing : sig
    (** [is_exclusive t] determines whether the mutex [t] is locked. *)
    val is_exclusive : 'k t @ local -> bool

    (** [length t] returns an upper bound on the length of the internal queue of awaiters
        for testing purposes. *)
    val length : 'k t @ local -> int
  end
end

module type Await = sig @@ portable
  open Await_kernel
  module Capsule := Capsule.Prim

  (** Mutexes that can be held for an unbounded period of time *)

  (** [k t] is the type of a mutex protecting the contents of the [k] capsule, which can
      be held for an unbounded period of time. *)
  type 'k t : value mod contended forkable non_float portable unyielding

  (** {1 Creating a mutex} *)

  (** [create k] creates a new mutex for the capsule ['k] associated with [k], consuming
      the key itself.

      The optional [padded] argument specifies whether to pad the data structure to avoid
      false sharing. See {!Atomic.make} for a longer explanation. *)
  val create : ?padded:bool @ local -> 'k Capsule.Key.t @ unique -> 'k t

  (** {1 Executing critical sections} *)

  (** {2 With uncontended [Access]} *)

  (** [with_access w t ~f] acquires [t], runs [f] within the associated capsule, then
      releases [t]. If [t] is locked then uses [w] to wait until it is unlocked.

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Terminated if [w] is terminated before the mutex is acquired. *)
  val with_access
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Access.t -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a @ contended once portable unique

  (** [try_with_access t ~f] is like {!with_access}, but returns [Would_block] if [t] is
      currently locked. *)
  val try_with_access
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Access.t -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a Or_would_block.t @ contended once portable unique

  (** [with_access_poisoning w t ~f] acquires [t], runs [f] within the associated capsule,
      then releases [t]. If [t] is locked then uses [w] to wait until it is unlocked.

      Poisons [t] if [f] raises an uncaught exception.

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Terminated if [w] is terminated before the mutex is acquired. *)
  val with_access_poisoning
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Access.t -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a @ contended once portable unique

  (** [try_with_access_poisoning t ~f] is like {!with_access_poisoning}, but returns
      [Would_block] if [t] is currently locked. *)
  val try_with_access_poisoning
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Access.t -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a Or_would_block.t @ contended once portable unique

  (** [with_access_or_cancel w c t f] is [Completed (with_access w t f)] if [c] is not
      canceled, otherwise it is [Canceled].

      @raise Terminated if [w] is terminated, even if [c] is canceled. *)
  val with_access_or_cancel
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Access.t -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a Or_canceled.t @ contended once portable unique

  (** [with_access_or_cancel_poisoning w c t f] is
      [Completed (with_access_poisoning w t f)] if [c] is not canceled, otherwise it is
      [Canceled].

      Poisons [t] if [f] raises an uncaught exception.

      @raise Terminated if [w] is terminated, even if [c] is canceled. *)
  val with_access_or_cancel_poisoning
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Access.t -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a Or_canceled.t @ contended once portable unique

  (** {2 With a local [Password]} *)

  (** [with_password w t f] acquires [t], runs [f] with permission to access the
      associated capsule, then releases [t]. If [t] is locked then uses [w] to wait until
      it is unlocked.

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Terminated if [w] is terminated before the mutex is acquired. *)
  val with_password
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
    -> 'a @ unique

  (** [try_with_password t ~f] is like {!with_password}, but returns [Would_block] if [t]
      is currently locked. *)
  val try_with_password
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
    -> 'a Or_would_block.t @ unique

  (** [with_password_poisoning w t f] acquires [t], runs [f] with permission to access the
      associated capsule, then releases [t]. If [t] is locked then uses [w] to wait until
      it is unlocked.

      Poisons [t] if [f] raises an exception.

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Terminated if [w] is terminated before the mutex is acquired. *)
  val with_password_poisoning
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
    -> 'a @ unique

  (** [try_with_password_poisoning t ~f] is like {!with_password_poisoning}, but returns
      [Would_block] if [t] is currently locked. *)
  val try_with_password_poisoning
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
    -> 'a Or_would_block.t @ unique

  (** [with_password_or_cancel w c t f] is [Completed (with_password w t f)] if [c] is not
      canceled, otherwise it is [Canceled].

      @raise Terminated if [w] is terminated, even if [c] is canceled. *)
  val with_password_or_cancel
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
    -> 'a Or_canceled.t @ unique

  (** [with_password_or_cancel_poisoning w c t f] is
      [Completed (with_password_poisoning w t f)] if [c] is not canceled, otherwise it is
      [Canceled].

      Poisons [t] if [f] raises an exception.

      @raise Terminated if [w] is terminated, even if [c] is canceled. *)
  val with_password_or_cancel_poisoning
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
    -> 'a Or_canceled.t @ unique

  (** {2 With a unique [Key]} *)

  [%%template:
  [@@@mode.default l = (global, local)]

  (** [with_key_poisoning t ~f] locks [t] and runs [f], providing it a key for ['k]
      uniquely. If the function raises without returning the key back, the mutex is
      poisoned. If [t] is locked, then [with_key_poisoning] uses [w] to wait until it is
      unlocked.

      There is no non-poisoning version of this function, because you must return the key
      back (to avoid leaking it via an exception).

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Terminated if [w] is terminated before the mutex is acquired. *)
  val with_key_poisoning
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
       @ local once
    -> 'a @ l once unique

  (** [try_with_key t ~f] is like {!with_key}, but returns [Would_block] if [t] is
      currently locked. *)
  val try_with_key_poisoning
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
       @ local once
    -> 'a Or_would_block.t @ l once unique

  (** [with_key_or_cancel_poisoning w c t ~f] is [Completed (with_key_poisoning w t ~f)]
      if [c] is not canceled, otherwise it is [Canceled].

      @raise Terminated if [w] is terminated, even if [c] is canceled. *)
  val with_key_or_cancel_poisoning
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
       @ local once
    -> 'a Or_canceled.t @ l once unique]

  (** {3 And waiting for a [Condition]} *)

  module Condition : Condition.S with type 'k lock := 'k t (** @open *)

  [%%template:
  [@@@mode.default l = (global, local)]

  (** [with_key_and_condition_wait_poisoning w t ~f] locks [t] and runs [f], providing it
      with a key for ['k] uniquely and a ['k Condition.Wait.t] which provides the ability
      to temporarily unlock [t] and wait on an associated ['k Condition.t].

      The lock is only poisoned if [f] raises {i while the lock held}; if waiting on a
      condition variable is terminated or if the callback passed to
      {!Condition.Wait.release_temporarily} raises, the lock will not be poisoned.

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Terminated if [w] is terminated before the mutex is acquired. *)
  val with_key_and_condition_wait_poisoning
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> 'k t @ local
    -> f:
         ('k Condition.Wait.t @ local
          -> 'k Capsule.Key.t @ unique
          -> #('a * 'k Capsule.Key.t) @ l once unique)
       @ local once
    -> 'a @ l once unique

  (** [with_key_and_condition_wait_or_cancel_poisoining w c t ~f] is
      [Completed (with_key_and_condition_wait_poisoining w t ~f)] if [c] is not canceled,
      otherwise it is [Canceled]. *)
  val with_key_and_condition_wait_or_cancel_poisoning
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:
         ('k Condition.Wait.t @ local
          -> 'k Capsule.Key.t @ unique
          -> #('a * 'k Capsule.Key.t) @ l once unique)
       @ local once
    -> 'a Or_canceled.t @ l once unique]

  (** {3 And releasing temporarily} *)

  (** [release_temporarily w t k ~f] releases [t], runs [f] without the mutex, reacquires
      [t] and returns the result of running [f] along with the [k].

      To obtain a key to pass to [release_temporarily], use {!with_key} or similar
      functions.

      @raise Poisoned if [t] cannot be reacquired because it is poisoned.
      @raise Terminated if [w] is terminated before the mutex is reacquired. *)
  val release_temporarily
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> 'k t @ local
    -> 'k Capsule.Key.t @ unique
    -> f:(unit -> 'a @ unique) @ local once
    -> #('a * 'k Capsule.Key.t) @ unique

  (** [release_temporarily_or_cancel w c t k ~f] is
      [Completed (release_temporarily w t k ~f)] if [c] is not canceled, otherwise it is
      [Canceled]. *)
  val release_temporarily_or_cancel
    : ('a : value_or_null).
    Await.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> 'k Capsule.Key.t @ unique
    -> f:(unit -> 'a @ unique) @ local once
    -> (#('a * 'k Capsule.Key.t) Or_canceled.t[@kind value_or_null & void]) @ unique

  (** {3 And poisoning indefinitely} *)

  (** [poison t key] poisons the mutex associated with the [key].

      Note that poisoning a mutex does not signal waiters on associated {{!Condition}
      condition variables}. *)
  val poison : 'k t @ local -> 'k Capsule.Key.t @ unique -> 'k Capsule.Key.t @ unique

  (** {2 Indefinitely through poisoning} *)

  (** [acquire_and_poison w t] acquires [t] and then immediately poisons it, returning the
      key to the protected capsule.

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Terminated if [w] is terminated before the mutex is acquired. *)
  val acquire_and_poison : Await.t @ local -> 'k t @ local -> 'k Capsule.Key.t @ unique

  (** [acquire_and_poison_or_cancel w c t] is [Completed (acquire_and_poison w t)] if [c]
      is not canceled, otherwise it is [Canceled]. *)
  val acquire_and_poison_or_cancel
    :  Await.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> ('k Capsule.Key.t Or_canceled.t[@kind void]) @ unique

  (** {2 With arbitrary dynamic scope} *)

  module Guard : sig
    (** ['k Mutex.Guard.t] represents a locked mutex. It morally contains a
        {!Capsule.Key.t}, but also has a finalizer that poisons the mutex if it is garbage
        collected without {!release} being called. *)
    type 'k t : value mod contended many portable

    (** [with_key t ~f] runs [f], providing it a key for ['k] uniquely. If the function
        raises without returning the key back, the mutex is poisoned. *)
    val with_key
      : ('a : value_or_null) 'k.
      'k t @ unique
      -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ once unique)
         @ local once
      -> 'a * 'k t @ once unique

    (** [with_password g ~f] runs [f], providing it a password for ['k], and returns the
        result of [f] together with the guard.

        If [f] raises an exception, the mutex is poisoned, and the exception is reraised. *)
    val with_password
      :  'k t @ unique
      -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
      -> 'a * 'k t @ unique

    (** [access g ~f] runs [f], providing it access to the capsule ['k], and returns the
        result of [f] together with the guard.

        If [f] raises an exception, the mutex is poisoned, and the exception is reraised. *)
    val access
      : ('a : value_or_null) 'k.
      'k t @ unique
      -> f:('k Capsule.Access.t -> 'a @ contended once portable unique)
         @ local once portable
      -> 'a * 'k t @ contended once portable unique

    (** [release guard] releases the mutex protected by [guard], consuming the guard. *)
    val release : 'k t @ unique -> unit

    (** [poison guard] poisons the mutex protected by [guard], consuming the guard and
        returning the key to the protected capsule.

        Note that poisoning a mutex does not signal waiters on associated {{!Condition}
        condition variables}. *)
    val poison : 'k t @ unique -> 'k Capsule.Key.t @ unique

    (** [is_poisoning guard] is [true] if [guard] will poison if its finalizer runs
        without [release] being called. *)
    val is_poisoning : 'k t @ local -> bool
  end

  (** [acquire w t] acquires [t] and returns a guard for the locked mutex. If [t] is
      locked, then acquire uses [w] to wait until it is unlocked.

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Terminated if [w] is terminated before the mutex is acquired. *)
  val acquire : Await.t @ local -> 'k t -> 'k Guard.t @ unique

  (** [acquire_or_cancel w c t] is [Completed (acquire w t)] if [c] is not canceled,
      otherwise it is [Canceled]. *)
  val acquire_or_cancel
    :  Await.t @ local
    -> Cancellation.t @ local
    -> 'k t
    -> 'k Guard.t Or_canceled.t @ unique

  (** [try_acquire t] is like {!acquire}, but returns [Would_block] if [t] is currently
      locked. *)
  val try_acquire : 'k t -> 'k Guard.t Or_would_block.t @ unique

  (** {1 Poisoning as a signal} *)

  (** [is_poisoned t] determines whether the mutex [t] is poisoned. *)
  val is_poisoned : 'k t @ local -> bool

  (** [is_locked t] determines whether the mutex [t] is locked. *)
  val is_locked : 'k t @ local -> bool

  (** [poison_unacquired t] poisons [t] without acquiring it. Does nothing if [t] is
      already poisoned.

      Note that this can render the capsule associated with [t] innaccessible, since it
      potentially destroys the associated key.

      Note that poisoning a mutex does not signal waiters on associated {{!Condition}
      condition variables}. *)
  val poison_unacquired : 'k t @ local -> unit

  (**/**)

  module For_testing : sig
    (** [is_exclusive t] determines whether the mutex [t] is locked. *)
    val is_exclusive : 'k t @ local -> bool

    (** [length t] returns an upper bound on the length of the internal queue of awaiters
        for testing purposes. *)
    val length : 'k t @ local -> int
  end
end

module type Mutex_common = sig @@ portable
  module Make_sync (Prim : Prim) : Sync with type 'k t = Prim.t
  module Make_await (Prim : Prim) : Await with type 'k t = Prim.t
end

module type With_state = sig @@ portable
  (** Here is an example of a counter protected by a mutex embedded within an atomic
      location:

      {[
        module Mutex = Await.Sync.Mutex

        type t =
          | T :
              { mutable mutex : 'k Mutex.Loc.State.t [@atomic]
              ; state : (int ref, 'k) Capsule.Data.t
              }
              -> t

        let make (value : int) =
          let (P key) = Capsule.Prim.create () in
          T
            { mutex = Mutex.Loc.make key
            ; state = Capsule.Data.create (fun () -> ref value)
            }
        ;;

        let fetch_and_add sync (T t) delta =
          Mutex.Loc.with_access sync [%atomic.loc t.mutex] ~f:(fun _ access : int ->
            let counter = Capsule.Data.unwrap ~access t.state in
            let value = !counter in
            counter := value + delta;
            value)
          [@nontail]
        ;;
      ]}

      Note the use of [[@atomic]] to specify an atomic location and [[%atomic.loc _]] to
      get a reference to the atomic location. *)

  module State : sig
    (** Internal state of a mutex location. *)

    (** ['k t] represents the state of a mutex location protecting the key to the capsule
        [k]. *)
    type 'k t : immediate
  end
end

module type Sync_loc = sig @@ portable
  include With_state (** @inline *)

  include Sync with type 'k t = 'k State.t Atomic.Loc.t (** @inline *)

  (** {1 Initializing a mutex embedded within an atomic location} *)

  (** [make key] computes the initial state for a mutex location. *)
  val make : 'k Capsule.Prim.Key.t @ unique -> 'k State.t
end

module type Await_loc = sig @@ portable
  include With_state (** @inline *)

  include Await with type 'k t = 'k State.t Atomic.Loc.t (** @inline *)

  (** {1 Initializing a mutex embedded within an atomic location} *)

  (** [make key] computes the initial state for a mutex location. *)
  val make : 'k Capsule.Prim.Key.t @ unique -> 'k State.t
end

module type Mutex = sig @@ portable
  module Sync : sig
    (** A poisonable mutual exclusion lock. *)

    (** {1 Freestanding mutex} *)

    include Sync (** @inline *)

    (** {1 Mutex embedded within an atomic location} *)

    (** A poisonable single word mutual exclusion lock embedded within an atomic location. *)
    module Loc : Sync_loc
  end

  module Await : sig
    (** A poisonable mutual exclusion lock. *)

    (** {1 Freestanding mutex} *)

    include Await (** @inline *)

    (** {1 Mutex embedded within an atomic location} *)

    (** A poisonable single word mutual exclusion lock embedded within an atomic location. *)

    module Loc : Await_loc
  end
end
