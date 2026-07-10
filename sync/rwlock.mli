@@ portable

open Await_kernel
module Capsule := Capsule.Prim

module Sync : sig
  (** A poisonable and freezable multiple readers, single writer lock that can only be
      held for a bounded period of time. *)

  (** [k t] is the type of a read/write lock protecting the contents of the [k] capsule. *)
  type 'k t : value mod contended non_float portable

  (** {1 Creating a reader-writer lock} *)

  (** [create k] creates a new reader-writer lock for the capsule ['k] associated with
      [k], consuming the key itself.

      The optional [padded] argument specifies whether to pad the data structure to avoid
      false sharing. See {!Atomic.make} for a longer explanation. *)
  val create : ?padded:bool @ local -> 'k Capsule.Key.t @ unique -> 'k t

  (** {1 Executing critical sections} *)

  (** {2 With [Access]} *)

  (** {3 Shared by readers} *)

  (** [with_access_shared w t ~f] acquires [t] for reading, runs [f] with shared access to
      the associated capsule, then releases [t]. If [t] is locked for writing then uses
      [w] to wait until it is unlocked. The [Sync.t] is passed to [f] to allow further use
      of synchronization primitives.

      @raise Poisoned if [t] cannot be acquired because it is poisoned. *)
  val with_access_shared
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> 'k t @ local
    -> f:
         (Sync.t @ local
          -> 'k Capsule.Access.t @ shared
          -> 'a @ contended once portable unique)
       @ local once portable unyielding
    -> 'a @ contended once portable unique

  (** [with_access_shared_freezing w t ~f] is [with_access_shared w t ~f], but freezes [t]
      if [f] raises an uncaught exception. *)
  val with_access_shared_freezing
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> 'k t @ local
    -> f:
         (Sync.t @ local
          -> 'k Capsule.Access.t @ shared
          -> 'a @ contended once portable unique)
       @ local once portable unyielding
    -> 'a @ contended once portable unique

  (** [try_with_access_shared t ~f] attempts to acquire [t] for reading without blocking.
      If successful, it runs [f] as [with_access_shared] would, but without taking a
      [Sync.t] because no waiting is needed. Otherwise, it is [Would_block]. *)
  val try_with_access_shared
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Access.t @ shared -> 'a @ contended once portable unique)
       @ local once portable unyielding
    -> 'a Or_would_block.t @ contended once portable unique

  (** [try_with_access_shared_freezing t ~f] is [try_with_access_shared t ~f], but freezes
      [t] if [f] raises an uncaught exception. *)
  val try_with_access_shared_freezing
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Access.t @ shared -> 'a @ contended once portable unique)
       @ local once portable unyielding
    -> 'a Or_would_block.t @ contended once portable unique

  (** [with_access_shared_or_cancel w c t ~f] is [Completed (with_access_shared w t ~f)]
      if [c] is not canceled, otherwise it is [Canceled]. *)
  val with_access_shared_or_cancel
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:
         (Sync.t @ local
          -> 'k Capsule.Access.t @ shared
          -> 'a @ contended once portable unique)
       @ local once portable unyielding
    -> 'a Or_canceled.t @ contended once portable unique

  (** [with_access_shared_or_cancel_freezing w c t ~f] is
      [with_access_shared_or_cancel w c t ~f], but freezes [t] if [f] raises an uncaught
      exception. *)
  val with_access_shared_or_cancel_freezing
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:
         (Sync.t @ local
          -> 'k Capsule.Access.t @ shared
          -> 'a @ contended once portable unique)
       @ local once portable unyielding
    -> 'a Or_canceled.t @ contended once portable unique

  (** {3 Uncontended for a writer} *)

  (** [with_access w t ~f] acquires [t] for writing, runs [f] within the associated
      capsule, then releases [t]. If [t] is locked then uses [w] to wait until it is
      unlocked. The [Sync.t] is passed to [f] to allow further use of synchronization
      primitives.

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Frozen if [t] cannot be acquired for writing because it is frozen. *)
  val with_access
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> 'k t @ local
    -> f:(Sync.t @ local -> 'k Capsule.Access.t -> 'a @ contended once portable unique)
       @ local once portable unyielding
    -> 'a @ contended once portable unique

  (** [with_access_poisoning w t ~f] is [with_access w t ~f], but poisons [t] if [f]
      raises an uncaught exception. *)
  val with_access_poisoning
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> 'k t @ local
    -> f:(Sync.t @ local -> 'k Capsule.Access.t -> 'a @ contended once portable unique)
       @ local once portable unyielding
    -> 'a @ contended once portable unique

  (** [try_with_access t ~f] attempts to acquire [t] for writing without blocking. If
      successful, it runs [f] as [with_access] would, but without taking a [Sync.t]
      because no waiting is needed. Otherwise, it is [Would_block]. *)
  val try_with_access
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Access.t -> 'a @ contended once portable unique)
       @ local once portable unyielding
    -> 'a Or_would_block.t @ contended once portable unique

  (** [try_with_access_poisoning t ~f] is [try_with_access t ~f], but poisons [t] if [f]
      raises an uncaught exception. *)
  val try_with_access_poisoning
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Access.t -> 'a @ contended once portable unique)
       @ local once portable unyielding
    -> 'a Or_would_block.t @ contended once portable unique

  (** [with_access_or_cancel w c t ~f] is [Completed (with_access w t ~f)] if [c] is not
      canceled, otherwise it is [Canceled]. *)
  val with_access_or_cancel
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:(Sync.t @ local -> 'k Capsule.Access.t -> 'a @ contended once portable unique)
       @ local once portable unyielding
    -> 'a Or_canceled.t @ contended once portable unique

  (** [with_access_or_cancel_poisoning w c t ~f] is [with_access_or_cancel w c t ~f], but
      poisons [t] if [f] raises an uncaught exception. *)
  val with_access_or_cancel_poisoning
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:(Sync.t @ local -> 'k Capsule.Access.t -> 'a @ contended once portable unique)
       @ local once portable unyielding
    -> 'a Or_canceled.t @ contended once portable unique

  (** {2 With a local [Password]} *)

  (** {3 [Password.Shared] by readers} *)

  (** [with_password_shared w t ~f] acquires [t], runs [f] with permission to access the
      associated capsule, then releases [t]. If [t] is locked then uses [w] to wait until
      it is unlocked. The [Sync.t] is passed to [f] to allow further use of
      synchronization primitives.

      @raise Poisoned if [t] cannot be acquired because it is poisoned. *)
  val with_password_shared
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> 'k t @ local
    -> f:(Sync.t @ local -> 'k Capsule.Password.Shared.t @ forkable local -> 'a @ unique)
       @ local once unyielding
    -> 'a @ unique

  (** [with_password_shared_freezing w t ~f] is [with_password_shared w t ~f], but freezes
      [t] if [f] raises an uncaught exception. *)
  val with_password_shared_freezing
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> 'k t @ local
    -> f:(Sync.t @ local -> 'k Capsule.Password.Shared.t @ forkable local -> 'a @ unique)
       @ local once unyielding
    -> 'a @ unique

  (** [try_with_password_shared t ~f] attempts to acquire [t] for reading without
      blocking. If successful, it runs [f] as [with_password_shared] would, but without
      taking a [Sync.t] because no waiting is needed. Otherwise, it is [Would_block]. *)
  val try_with_password_shared
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Password.Shared.t @ forkable local -> 'a @ unique)
       @ local once unyielding
    -> 'a Or_would_block.t @ unique

  (** [try_with_password_shared_freezing t ~f] is [try_with_password_shared t ~f], but
      freezes [t] if [f] raises an uncaught exception. *)
  val try_with_password_shared_freezing
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Password.Shared.t @ forkable local -> 'a @ unique)
       @ local once unyielding
    -> 'a Or_would_block.t @ unique

  (** [with_password_shared_or_cancel w c t ~f] is
      [Completed (with_password_shared w t ~f)] if [c] is not canceled, otherwise it is
      [Canceled]. *)
  val with_password_shared_or_cancel
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:(Sync.t @ local -> 'k Capsule.Password.Shared.t @ forkable local -> 'a @ unique)
       @ local once unyielding
    -> 'a Or_canceled.t @ unique

  (** [with_password_shared_or_cancel_freezing w c t ~f] is
      [with_password_shared_or_cancel w t ~f], but freezes [t] if [f] raises an uncaught
      exception. *)
  val with_password_shared_or_cancel_freezing
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:(Sync.t @ local -> 'k Capsule.Password.Shared.t @ forkable local -> 'a @ unique)
       @ local once unyielding
    -> 'a Or_canceled.t @ unique

  (** {3 [Password] for a writer} *)

  (** [with_password w t ~f] acquires [t], runs [f] with permission to access the
      associated capsule, then releases [t]. If [t] is locked then uses [w] to wait until
      it is unlocked. The [Sync.t] is passed to [f] to allow further use of
      synchronization primitives.

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Frozen if [t] cannot be acquired for writing because it is frozen. *)
  val with_password
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> 'k t @ local
    -> f:(Sync.t @ local -> 'k Capsule.Password.t @ local -> 'a @ unique)
       @ local once unyielding
    -> 'a @ unique

  (** [with_password_poisoning w t ~f] is [with_password w t ~f], but poisons [t] if [f]
      raises an uncaught exception. *)
  val with_password_poisoning
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> 'k t @ local
    -> f:(Sync.t @ local -> 'k Capsule.Password.t @ local -> 'a @ unique)
       @ local once unyielding
    -> 'a @ unique

  (** [try_with_password t ~f] attempts to acquire [t] for writing without blocking. If
      successful, it runs [f] as [with_password] would, but without taking a [Sync.t]
      because no waiting is needed. Otherwise, it is [Would_block]. *)
  val try_with_password
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once unyielding
    -> 'a Or_would_block.t @ unique

  (** [try_with_password_poisoning t ~f] is [try_with_password t ~f], but poisons [t] if
      [f] raises an uncaught exception. *)
  val try_with_password_poisoning
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once unyielding
    -> 'a Or_would_block.t @ unique

  (** [with_password_or_cancel w c t ~f] is [Completed (with_password w t ~f)] if [c] is
      not canceled, otherwise it is [Canceled]. *)
  val with_password_or_cancel
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:(Sync.t @ local -> 'k Capsule.Password.t @ local -> 'a @ unique)
       @ local once unyielding
    -> 'a Or_canceled.t @ unique

  (** [with_password_or_cancel_poisoning w c t ~f] is [with_password_or_cancel w c t ~f],
      but poisons [t] if [f] raises an uncaught exception. *)
  val with_password_or_cancel_poisoning
    : ('a : value_or_null) 'k.
    Sync.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:(Sync.t @ local -> 'k Capsule.Password.t @ local -> 'a @ unique)
       @ local once unyielding
    -> 'a Or_canceled.t @ unique

  (** {2 With a [Key]} *)

  [%%template:
  [@@@mode.default l = (global, local)]

  (** [with_key_poisoning s t ~f] locks [t] for writing and runs [f], providing it a key
      for ['k] uniquely. If [t] is locked for reading or writing, then [with_key] uses [s]
      to wait until it is unlocked. The [Sync.t] is passed to [f] to allow further use of
      synchronization primitives.

      There is no non-poisoning version of this function, because you must return the key
      back (to avoid leaking it via an exception).

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Frozen if [t] cannot be acquired for writing because it is frozen. *)
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

  (** [try_with_key_poisoning t ~f] attempts to acquire [t] for writing without blocking.
      If successful, it runs [f] as [with_key_poisoning] would, but without taking a
      [Sync.t] because no waiting is needed. Otherwise, it is [Would_block]. *)
  val try_with_key_poisoning
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
       @ local once unyielding
    -> 'a Or_would_block.t @ l once unique

  (** [with_key_or_cancel_poisoning w c t ~f] is [with_key_or_cancel w c t ~f], but
      poisons [t] if [f] raises an uncaught exception. *)
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

  (** {4 And waiting for a [Condition]} *)

  module Condition : Condition_intf.S with type 'k lock := 'k t (** @open *)

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

  (** {4 And poisoning indefinitely} *)

  (** [poison t key] poisons the rwlock associated with the [key].

      Note that poisoning a rwlock does not signal waiters on associated {{!Condition}
      condition variables}. *)
  val poison : 'k t @ local -> 'k Capsule.Key.t @ unique -> 'k Capsule.Key.t @ unique

  (** {1 Poisoning as a signal} *)

  (** [is_poisoned t] is [true] if the reader-writer lock [t] is poisoned. *)
  val is_poisoned : 'k t @ local -> bool

  (** [is_frozen t] is [true] if the reader-writer lock [t] is frozen. *)
  val is_frozen : 'k t @ local -> bool

  (**/**)

  module For_testing : sig
    (** [is_exclusive t] determines whether the rwlock [t] is acquired exclusively. *)
    val is_exclusive : 'k t @ local -> bool

    (** [is_shared t] determines whether the rwlock [t] is acquired for reading. *)
    val is_shared : 'k t @ local -> bool

    (** [length t] returns an upper bound on the length of the internal queue of awaiters
        for testing purposes. *)
    val length : 'k t @ local -> int
  end
end

module Await : sig
  (** A poisonable and freezable multiple readers, single writer lock that can be acquired
      for an unbounded period of time. *)

  (** [k t] is the type of a read/write lock protecting the contents of the [k] capsule. *)
  type 'k t : value mod contended non_float portable

  (** {1 Creating a reader-writer lock} *)

  (** [create k] creates a new reader-writer lock for the capsule ['k] associated with
      [k], consuming the key itself.

      The optional [padded] argument specifies whether to pad the data structure to avoid
      false sharing. See {!Atomic.make} for a longer explanation. *)
  val create : ?padded:bool @ local -> 'k Capsule.Key.t @ unique -> 'k t

  (** {1 Executing critical sections} *)

  (** {2 With [Access]} *)

  (** {3 Shared by readers} *)

  (** [with_access_shared w t ~f] acquires [t] for reading, runs [f] with shared access to
      the associated capsule, then releases [t]. If [t] is locked for writing then uses
      [w] to wait until it is unlocked.

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Terminated if [w] is terminated before the lock is acquired. *)
  val with_access_shared
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Access.t @ shared -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a @ contended once portable unique

  (** [with_access_shared_freezing w t ~f] is [with_access_shared w t ~f], but freezes [t]
      if [f] raises an uncaught exception. *)
  val with_access_shared_freezing
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Access.t @ shared -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a @ contended once portable unique

  (** [try_with_access_shared t ~f] attempts to acquire [t] for reading without blocking.
      If successful, it runs [f] as [with_access_shared] would, but without taking a
      [Await.t] because no waiting is needed. Otherwise, it is [Would_block]. *)
  val try_with_access_shared
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Access.t @ shared -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a Or_would_block.t @ contended once portable unique

  (** [try_with_access_shared_freezing t ~f] is [try_with_access_shared t ~f], but freezes
      [t] if [f] raises an uncaught exception. *)
  val try_with_access_shared_freezing
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Access.t @ shared -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a Or_would_block.t @ contended once portable unique

  (** [with_access_shared_or_cancel w c t ~f] is [Completed (with_access_shared w t ~f)]
      if [c] is not canceled, otherwise it is [Canceled]. *)
  val with_access_shared_or_cancel
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Access.t @ shared -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a Or_canceled.t @ contended once portable unique

  (** [with_access_shared_or_cancel_freezing w c t ~f] is
      [with_access_shared_or_cancel w c t ~f], but freezes [t] if [f] raises an uncaught
      exception. *)
  val with_access_shared_or_cancel_freezing
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Access.t @ shared -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a Or_canceled.t @ contended once portable unique

  (** {3 Uncontended for a writer} *)

  (** [with_access w t ~f] acquires [t] for writing, runs [f] within the associated
      capsule, then releases [t]. If [t] is locked then uses [w] to wait until it is
      unlocked.

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Frozen if [t] cannot be acquired for writing because it is frozen.
      @raise Terminated if [w] is terminated before the lock is acquired. *)
  val with_access
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Access.t -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a @ contended once portable unique

  (** [with_access_poisoning w t ~f] is [with_access w t ~f], but poisons [t] if [f]
      raises an uncaught exception. *)
  val with_access_poisoning
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Access.t -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a @ contended once portable unique

  (** [try_with_access t ~f] attempts to acquire [t] for writing without blocking. If
      successful, it runs [f] as [with_access] would, but without taking an [Await.t]
      because no waiting is needed. Otherwise, it is [Would_block]. *)
  val try_with_access
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Access.t -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a Or_would_block.t @ contended once portable unique

  (** [try_with_access_poisoning t ~f] is [try_with_access t ~f], but poisons [t] if [f]
      raises an uncaught exception. *)
  val try_with_access_poisoning
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Access.t -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a Or_would_block.t @ contended once portable unique

  (** [with_access_or_cancel w c t ~f] is [Completed (with_access w t ~f)] if [c] is not
      canceled, otherwise it is [Canceled]. *)
  val with_access_or_cancel
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Access.t -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a Or_canceled.t @ contended once portable unique

  (** [with_access_or_cancel_poisoning w c t ~f] is [with_access_or_cancel w c t ~f], but
      poisons [t] if [f] raises an uncaught exception. *)
  val with_access_or_cancel_poisoning
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Access.t -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a Or_canceled.t @ contended once portable unique

  (** {2 With a local} *)

  (** {3 [Password.Shared] by readers} *)

  (** [with_password_shared w t ~f] acquires [t], runs [f] with permission to access the
      associated capsule, then releases [t]. If [t] is locked then uses [w] to wait until
      it is unlocked.

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Terminated if [w] is terminated before the lock is acquired. *)
  val with_password_shared
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Password.Shared.t @ forkable local -> 'a @ unique) @ local once
    -> 'a @ unique

  (** [with_password_shared_freezing w t ~f] is [with_password_shared w t ~f], but freezes
      [t] if [f] raises an uncaught exception. *)
  val with_password_shared_freezing
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Password.Shared.t @ forkable local -> 'a @ unique) @ local once
    -> 'a @ unique

  (** [try_with_password_shared t ~f] attempts to acquire [t] for reading without
      blocking. If successful, it runs [f] as [with_password_shared] would, but without
      taking an [Await.t] because no waiting is needed. Otherwise, it is [Would_block]. *)
  val try_with_password_shared
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Password.Shared.t @ forkable local -> 'a @ unique) @ local once
    -> 'a Or_would_block.t @ unique

  (** [try_with_password_shared_freezing t ~f] is [try_with_password_shared t ~f], but
      freezes [t] if [f] raises an uncaught exception. *)
  val try_with_password_shared_freezing
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Password.Shared.t @ forkable local -> 'a @ unique) @ local once
    -> 'a Or_would_block.t @ unique

  (** [with_password_shared_or_cancel w c t ~f] is
      [Completed (with_password_shared w t ~f)] if [c] is not canceled, otherwise it is
      [Canceled]. *)
  val with_password_shared_or_cancel
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Password.Shared.t @ forkable local -> 'a @ unique) @ local once
    -> 'a Or_canceled.t @ unique

  (** [with_password_shared_or_cancel_freezing w c t ~f] is
      [with_password_shared_or_cancel w t ~f], but freezes [t] if [f] raises an uncaught
      exception. *)
  val with_password_shared_or_cancel_freezing
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Password.Shared.t @ forkable local -> 'a @ unique) @ local once
    -> 'a Or_canceled.t @ unique

  (** {3 [Password] for a writer} *)

  (** [with_password w t ~f] acquires [t], runs [f] with permission to access the
      associated capsule, then releases [t]. If [t] is locked then uses [w] to wait until
      it is unlocked.

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Frozen if [t] cannot be acquired for writing because it is frozen.
      @raise Terminated if [w] is terminated before the lock is acquired. *)
  val with_password
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
    -> 'a @ unique

  (** [with_password_poisoning w t ~f] is [with_password w t ~f], but poisons [t] if [f]
      raises an uncaught exception. *)
  val with_password_poisoning
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
    -> 'a @ unique

  (** [try_with_password t ~f] attempts to acquire [t] for writing without blocking. If
      successful, it runs [f] as [with_password] would, but without taking an [Await.t]
      because no waiting is needed. Otherwise, it is [Would_block]. *)
  val try_with_password
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
    -> 'a Or_would_block.t @ unique

  (** [try_with_password_poisoning t ~f] is [try_with_password t ~f], but poisons [t] if
      [f] raises an uncaught exception. *)
  val try_with_password_poisoning
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
    -> 'a Or_would_block.t @ unique

  (** [with_password_or_cancel w c t ~f] is [Completed (with_password w t ~f)] if [c] is
      not canceled, otherwise it is [Canceled]. *)
  val with_password_or_cancel
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
    -> 'a Or_canceled.t @ unique

  (** [with_password_or_cancel_poisoning w c t ~f] is [with_password_or_cancel w c t ~f],
      but poisons [t] if [f] raises an uncaught exception. *)
  val with_password_or_cancel_poisoning
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
    -> 'a Or_canceled.t @ unique

  (** {2 With a [Key]} *)

  [%%template:
  [@@@mode.default l = (global, local)]

  (** [with_key_poisoning w t ~f] is [with_key w t ~f], but poisons [t] if [f] raises an
      uncaught exception. *)
  val with_key_poisoning
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
       @ local once
    -> 'a @ l once unique

  (** [try_with_key_poisoning t ~f] attempts to acquire [t] for writing without blocking.
      If successful, it runs [f] as [with_key_poisoning] would, but without taking a
      [Await.t] because no waiting is needed. Otherwise, it is [Would_block]. *)
  val try_with_key_poisoning
    : ('a : value_or_null) 'k.
    'k t @ local
    -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
       @ local once
    -> 'a Or_would_block.t @ l once unique

  (** [with_key_or_cancel_poisoning w c t ~f] is [with_key_or_cancel w c t ~f], but
      poisons [t] if [f] raises an uncaught exception. *)
  val with_key_or_cancel_poisoning
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
       @ local once
    -> 'a Or_canceled.t @ l once unique]

  (** {4 And waiting for a [Condition]} *)

  module Condition : Condition_intf.S with type 'k lock := 'k t (** @open *)

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

  (** {4 And poisoning indefinitely} *)

  (** [poison t key] poisons the rwlock associated with the [key].

      Note that poisoning a rwlock does not signal waiters on associated {{!Condition}
      condition variables}. *)
  val poison : 'k t @ local -> 'k Capsule.Key.t @ unique -> 'k Capsule.Key.t @ unique

  (** {2 With arbitrary dynamic scope} *)

  (** {3 For readers} *)

  module Shared_guard : sig
    (** ['k Rwlock.Shared_guard.t] represents a reader-writer locked that is locked
        non-exclusively for reading. It morally contains a [Capsule.Key.t @@ aliased], but
        also has a finalizer that freezes the lock if the guard is garbage collected
        without being passed to {!Shared_guard.release}. *)
    type 'k t : value mod contended portable

    (** [with_key guard ~f] runs [f], providing it an aliased key for ['k], and returns
        the result of [f] together with the shared guard. *)
    val with_key
      : ('a : value_or_null) 'k.
      'k t @ unique
      -> f:('k Capsule.Key.t -> 'a @ once unique) @ local once
      -> 'a * 'k t @ once unique

    (** [with_password g ~f] runs [f], providing it a shared password for ['k], and
        returns the result of [f] together with the shared guard. *)
    val with_password
      : ('a : value_or_null) 'k.
      'k t @ unique
      -> f:('k Capsule.Password.Shared.t @ forkable local -> 'a @ unique) @ local once
      -> 'a * 'k t @ unique

    (** [access g ~f] runs [f], providing it shared access to the capsule ['k], and
        returns the result of [f] together with the shared guard. *)
    val access
      : ('a : value_or_null) 'k.
      'k t @ unique
      -> f:('k Capsule.Access.t @ shared -> 'a @ contended once portable unique)
         @ local once portable
      -> 'a * 'k t @ contended once portable unique

    (** [release guard] releases the read lock *)
    val release : 'k t @ unique -> unit

    (** [freeze guard] marks the lock protected by the read guard [guard] as frozen,
        preventing later attempts to acquire it for writing, and returns an aliased key to
        the capsule. *)
    val freeze : 'k t @ unique -> 'k Capsule.Key.t
  end

  (** [acquire_shared w t] acquires [t] for reading and returns a {!Shared_guard.t} for
      the lock. If [t] is already locked for writing, [acquire_shared] uses [w] to block
      until it is only locked for reading.

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Terminated if [w] is terminated before the lock is acquired. *)
  val acquire_shared : Await.t @ local -> 'k t -> 'k Shared_guard.t @ unique

  (** [try_acquire_shared t] attempts to acquire [t] for reading without blocking, and
      does not take an [Await.t] because no waiting is needed. It is [Would_block] if [t]
      is locked for writing.

      @raise Poisoned if [t] cannot be acquired because it is poisoned. *)
  val try_acquire_shared : 'k t -> 'k Shared_guard.t Or_would_block.t @ unique

  (** [acquire_shared_or_cancel w c t] is [Completed (acquire_shared w t)] if [c] is not
      canceled, otherwise it is [Canceled].

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Terminated
        if [w] is terminated before the lock is acquired, even if [c] is canceled. *)
  val acquire_shared_or_cancel
    :  Await.t @ local
    -> Cancellation.t @ local
    -> 'k t
    -> 'k Shared_guard.t Or_canceled.t @ unique

  (** {3 For a writer} *)

  module Guard : sig
    (** ['k Rwlock.Guard.t] represents a reader-writer lock that is locked exclusively for
        writing. It morally contains a {!Capsule.Key.t}, but also has a finalizer that
        poisons the lock if the guard is garbage collected without being passed to
        {!Guard.release}. *)
    type 'k t : value mod contended portable

    (** [with_key t ~f] runs [f], providing it a key for ['k] uniquely. *)
    val with_key
      : ('a : value_or_null) 'k.
      'k t @ unique
      -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ once unique)
         @ local once
      -> 'a * 'k t @ once unique

    (** [with_password g ~f] runs [f], providing it a password for ['k], and returns the
        result of [f] together with the guard. *)
    val with_password
      : ('a : value_or_null) 'k.
      'k t @ unique
      -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
      -> 'a * 'k t @ unique

    (** [access g ~f] runs [f], providing it access to the capsule ['k], and returns the
        result of [f] together with the guard. *)
    val access
      : ('a : value_or_null) 'k.
      'k t @ unique
      -> f:('k Capsule.Access.t -> 'a @ contended once portable unique)
         @ local once portable
      -> 'a * 'k t @ contended once portable unique

    (** [release guard] releases the lock protected by [guard], consuming the guard. *)
    val release : 'k t @ unique -> unit

    (** [poison guard] poisons the lock protected by [guard], consuming the guard and
        returning the key to the protected capsule. *)
    val poison : 'k t @ unique -> 'k Capsule.Key.t @ unique

    (** [downgrade guard] downgrades the write guard [guard] to a read guard. *)
    val downgrade : 'k t @ unique -> 'k Shared_guard.t @ unique
  end

  (** [acquire w t] acquires [t] for writing and returns a {!Write_guard.t} for the lock.
      If [t] is already locked for reading or writing, [acquire] uses [w] to block until
      it is unlocked.

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Frozen if [t] cannot be acquired for writing because it is frozen.
      @raise Terminated if [w] is terminated before the lock is acquired. *)
  val acquire : Await.t @ local -> 'k t -> 'k Guard.t @ unique

  (** [try_acquire t] attempts to acquire [t] for writing without blocking, and does not
      take an [Await.t] because no waiting is needed. It is [Would_block] if [t] is
      locked.

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Frozen if [t] cannot be acquired for writing because it is frozen. *)
  val try_acquire : 'k t -> 'k Guard.t Or_would_block.t @ unique

  (** [acquire_or_cancel w c t] is [Completed (acquire w t)] if [c] is not canceled,
      otherwise it is [Canceled].

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Frozen if [t] cannot be acquired for writing because it is frozen.
      @raise Terminated
        if [w] is terminated before the lock is acquired, even if [c] is canceled. *)
  val acquire_or_cancel
    :  Await.t @ local
    -> Cancellation.t @ local
    -> 'k t
    -> 'k Guard.t Or_canceled.t @ unique

  (** {1 Poisoning as a signal} *)

  (** [is_poisoned t] is [true] if the reader-writer lock [t] is poisoned. *)
  val is_poisoned : 'k t @ local -> bool

  (** [is_frozen t] is [true] if the reader-writer lock [t] is frozen. *)
  val is_frozen : 'k t @ local -> bool

  (**/**)

  module For_testing : sig
    (** [is_exclusive t] determines whether the rwlock [t] is acquired exclusively. *)
    val is_exclusive : 'k t @ local -> bool

    (** [is_shared t] determines whether the rwlock [t] is acquired for reading. *)
    val is_shared : 'k t @ local -> bool

    (** [length t] returns an upper bound on the length of the internal queue of awaiters
        for testing purposes. *)
    val length : 'k t @ local -> int
  end
end
