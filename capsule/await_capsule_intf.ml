open! Base
open Import

module Definitions (C : sig
    module Mutex : sig
      type 'k t
    end

    module Rwlock : sig
      type 'k t
    end
  end) =
struct
  module type Module_with_mutex = sig
    type k

    val mutex : k C.Mutex.t
  end

  module type Module_with_rwlock = sig
    type k

    val rwlock : k C.Rwlock.t
  end
end

module type Capsule = sig @@ portable
  include module type of struct
    include Capsule
  end

  (** Locks protecting explicit capsules which take a {!Sync.t}, and may only be held for
      a bounded period of time.

      The [Sync.t] capability is passed to all callbacks to allow further use of
      synchronization primitives.

      You should prefer the primitives exported by this module over those exported by the
      [Await] module if you can, as they are easier to reason about and less likely to
      lead to deadlocks *)
  module Sync : sig
    include module type of struct
      include Definitions (Sync)
    end

    module Mutex : sig
      type 'k t = 'k Sync.Mutex.t
      type packed = P : 'k t -> packed

      (** Creates a mutex with a fresh existential key type. *)
      val create : unit -> packed

      (** Like [create]. Useful in module definitions, where GADTs cannot be unpacked. *)
      module Create () : Module_with_mutex

      (** [with_lock s t ~f] locks [t] and runs [f], providing it access to the capsule.
          The [Sync.t] is passed to [f] to allow further use of synchronization
          primitives. *)
      val with_lock
        : 'k 'a ('b : value_or_null).
        Sync.t @ local
        -> 'k t
        -> f:(Sync.t @ local -> 'k Capsule.Access.t -> 'a @ contended portable)
           @ local once portable unyielding
        -> 'a @ contended portable

      (** [with_lock_or_cancel s c t ~f] is [Completed (with_lock s t ~f)] if [c] is not
          canceled, otherwise it is [Canceled]. *)
      val with_lock_or_cancel
        : 'k 'a ('b : value_or_null).
        Sync.t @ local
        -> Cancellation.t @ local
        -> 'k t
        -> f:(Sync.t @ local -> 'k Capsule.Access.t -> 'a @ contended portable)
           @ local once portable unyielding
        -> 'a Or_canceled.t @ contended portable

      (** Functions that poison the mutex if the provided callback raises an exception. *)
      module Poisoning : sig
        (** [with_lock s t ~f] locks [t] and runs [f], providing it access to the capsule.
            The [Sync.t] is passed to [f] to allow further use of synchronization
            primitives.

            If [f] raises, [t] will be poisoned, meaning all subsequent attempts to
            acquire it will raise. *)
        val with_lock
          : 'k 'a ('b : value_or_null).
          Sync.t @ local
          -> 'k t
          -> f:(Sync.t @ local -> 'k Capsule.Access.t -> 'a @ contended portable)
             @ local once portable unyielding
          -> 'a @ contended portable

        (** [with_lock_or_cancel s c t ~f] is [Completed (with_lock s t ~f)] if [c] is not
            canceled, otherwise it is [Canceled].

            If [f] raises, [t] will be poisoned, meaning all subsequent attempts to
            acquire it will raise. *)
        val with_lock_or_cancel
          : 'k 'a ('b : value_or_null).
          Sync.t @ local
          -> Cancellation.t @ local
          -> 'k t
          -> f:(Sync.t @ local -> 'k Capsule.Access.t -> 'a @ contended portable)
             @ local once portable unyielding
          -> 'a Or_canceled.t @ contended portable
      end

      module Expert = Sync.Mutex
    end

    module With_mutex : sig
      (** An ['a Capsule.With_mutex.t] is a value of type ['a] in its own capsule,
          protected by a mutex *)
      type 'a t =
        | P :
            { data : ('a, 'k) Capsule.Data.t
            ; mutex : 'k Mutex.t
            }
            -> 'a t

      (** [create f] runs [f] within a fresh capsule, and creates a [Capsule.With_mutex.t]
          containing the result *)
      val create : (unit -> 'a) @ local once portable -> 'a t

      (** [of_isolated isolated] creates a [Capsule.With_mutex.t] from a value in an
          isolated capsule, consuming the isolated capsule. *)
      val of_isolated : 'a Capsule.Isolated.t @ unique -> 'a t

      (** [with_lock s t ~f] locks the mutex associated with [t] and calls [f] on the
          protected value, returning the result. The [Sync.t] is passed to [f] to allow
          further use of synchronization primitives. *)
      val with_lock
        : 'a ('b : value_or_null).
        Sync.t @ local
        -> 'a t
        -> f:(Sync.t @ local -> 'a -> 'b @ contended portable)
           @ local once portable unyielding
        -> 'b @ contended portable

      (** [with_lock_or_cancel s c t ~f] is [Completed (with_lock s t ~f)] if [c] is not
          canceled, otherwise it is [Canceled]. *)
      val with_lock_or_cancel
        : 'a ('b : value_or_null).
        Sync.t @ local
        -> Cancellation.t @ local
        -> 'a t
        -> f:(Sync.t @ local -> 'a -> 'b @ contended portable)
           @ local once portable unyielding
        -> 'b Or_canceled.t @ contended portable

      (** [with_guard sync t ~f] locks the mutex associated with [t] and calls [f] with
          the provided [sync] and a [Guard.t] for the protected data, returning the
          result.

          Since the provided callback does not have to be [portable], this is useful when
          you need to acquire two mutexes for different capsules at the same time and move
          data between them. For example:

          {[
            open! Core
            open! Await

            (* Lock both mutexes, and set the value of both refs to the maximum of their
               previous values, without any data races

               Make sure to always call this with locks in the same order, to avoid
               deadlocks! *)
            let set_to_max
              :  Sync.t @ local -> int ref Capsule.Sync.With_mutex.t
              -> int ref Capsule.Sync.With_mutex.t -> unit
              =
              fun sync ref1 ref2 ->
              Capsule.Sync.With_mutex.with_guard sync ref1 ~f:(fun sync guard1 ->
                Capsule.Sync.With_mutex.with_guard sync ref2 ~f:(fun _sync guard2 ->
                  (* At this point we have both mutexes locked, so we can freely
                     manipulate the data they protect without the potential for data races *)
                  let value1 = Capsule.Guard.get guard1 ~f:(fun r -> !r) in
                  let value2 = Capsule.Guard.get guard2 ~f:(fun r -> !r) in
                  let new_value = Int.max value1 value2 in
                  Capsule.Guard.iter guard1 ~f:(fun r -> r := new_value);
                  Capsule.Guard.iter guard2 ~f:(fun r -> r := new_value) [@nontail])
                [@nontail])
              [@nontail]
            ;;
          ]} *)
      val with_guard
        : 'a ('b : value_or_null).
        Sync.t @ local
        -> 'a t
        -> f:(Sync.t @ local -> 'a Guard.t @ local -> 'b) @ local once unyielding
        -> 'b

      (** [with_guard_or_cancel s c t ~f] is [Completed (with_guard s t ~f)] if [c] is not
          canceled, otherwise it is [Canceled]. *)
      val with_guard_or_cancel
        : 'a ('b : value_or_null).
        Sync.t @ local
        -> Cancellation.t @ local
        -> 'a t
        -> f:(Sync.t @ local -> 'a Guard.t @ local -> 'b) @ local once unyielding
        -> 'b Or_canceled.t

      (** Functions that poison the mutex if the provided callback raises an exception. *)
      module Poisoning : sig
        (** [with_lock s t ~f] locks the mutex associated with [t] and calls [f] on the
            protected value, returning the result. The [Sync.t] is passed to [f] to allow
            further use of synchronization primitives.

            If [f] raises, [t] will be poisoned, meaning all subsequent attempts to
            acquire it will raise. *)
        val with_lock
          : 'a ('b : value_or_null).
          Sync.t @ local
          -> 'a t
          -> f:(Sync.t @ local -> 'a -> 'b @ contended portable)
             @ local once portable unyielding
          -> 'b @ contended portable

        (** [with_lock_or_cancel s c t ~f] is [Completed (with_lock s t ~f)] if [c] is not
            canceled, otherwise it is [Canceled].

            If [f] raises, [t] will be poisoned, meaning all subsequent attempts to
            acquire it will raise. *)
        val with_lock_or_cancel
          : 'a ('b : value_or_null).
          Sync.t @ local
          -> Cancellation.t @ local
          -> 'a t
          -> f:(Sync.t @ local -> 'a -> 'b @ contended portable)
             @ local once portable unyielding
          -> 'b Or_canceled.t @ contended portable
      end

      (** [iter s t ~f] is [with_lock s t ~f], specialised to a function that returns
          [unit] *)
      val iter
        :  Sync.t @ local
        -> 'a t
        -> f:(Sync.t @ local -> 'a -> unit) @ local once portable unyielding
        -> unit

      (** [map s t ~f] locks the mutex associated with [t] and calls [f] on the protected
          value, returning a new [With_mutex.t] containing the result in the same capsule,
          and protected by the same mutex. The [Sync.t] is passed to [f] to allow further
          use of synchronization primitives. *)
      val map
        :  Sync.t @ local
        -> 'a t
        -> f:(Sync.t @ local -> 'a -> 'b) @ local once portable unyielding
        -> 'b t

      (** [destroy s t] poisons the mutex associated with [t], merging the protected value
          into the current capsule and returning it. *)
      val destroy : Sync.t @ local -> 'a t -> 'a
    end

    module Rwlock : sig
      type 'k t = 'k Sync.Rwlock.t
      type packed = P : 'k t -> packed

      (** Creates a reader-writer lock with a fresh existential key type. *)
      val create : unit -> packed

      (** Like [create]. Useful in module definitions, where GADTs cannot be unpacked. *)
      module Create () : Module_with_rwlock

      (** [with_write s t ~f] locks [t] for writing and runs [f], providing it access to
          the capsule. The [Sync.t] is passed to [f] to allow further use of
          synchronization primitives. *)
      val with_write
        : 'k 'a ('b : value_or_null).
        Sync.t @ local
        -> 'k t
        -> f:(Sync.t @ local -> 'k Capsule.Access.t -> 'a @ contended portable)
           @ local once portable unyielding
        -> 'a @ contended portable

      (** [with_write_or_cancel s c t ~f] is [Completed (with_write s t ~f)] if [c] is not
          canceled, otherwise it is [Canceled]. *)
      val with_write_or_cancel
        : 'k 'a ('b : value_or_null).
        Sync.t @ local
        -> Cancellation.t @ local
        -> 'k t
        -> f:(Sync.t @ local -> 'k Capsule.Access.t -> 'a @ contended portable)
           @ local once portable unyielding
        -> 'a Or_canceled.t @ contended portable

      (** [with_read s t ~f] locks [t] for reading and runs [f], providing it shared
          access to the capsule. The [Sync.t] is passed to [f] to allow further use of
          synchronization primitives. *)
      val with_read
        : 'k 'a ('b : value_or_null).
        Sync.t @ local
        -> 'k t
        -> f:(Sync.t @ local -> 'k Capsule.Access.t @ shared -> 'a @ contended portable)
           @ local once portable unyielding
        -> 'a @ contended portable

      (** [with_read_or_cancel s c t ~f] is [Completed (with_read s t ~f)] if [c] is not
          canceled, otherwise it is [Canceled]. *)
      val with_read_or_cancel
        : 'k 'a ('b : value_or_null).
        Sync.t @ local
        -> Cancellation.t @ local
        -> 'k t
        -> f:(Sync.t @ local -> 'k Capsule.Access.t @ shared -> 'a @ contended portable)
           @ local once portable unyielding
        -> 'a Or_canceled.t @ contended portable

      (** Functions that poison or freeze the lock if the provided callback raises an
          exception. *)
      module Poisoning : sig
        (** [with_write s t ~f] locks [t] for writing and runs [f], providing it access to
            the capsule. The [Sync.t] is passed to [f] to allow further use of
            synchronization primitives.

            If [f] raises, [t] will be poisoned, meaning all subsequent attempts to
            acquire it will raise. *)
        val with_write
          : 'k 'a ('b : value_or_null).
          Sync.t @ local
          -> 'k t
          -> f:(Sync.t @ local -> 'k Capsule.Access.t -> 'a @ contended portable)
             @ local once portable unyielding
          -> 'a @ contended portable

        (** [with_write_or_cancel s c t ~f] is [Completed (with_write s t ~f)] if [c] is
            not canceled, otherwise it is [Canceled]. *)
        val with_write_or_cancel
          : 'k 'a ('b : value_or_null).
          Sync.t @ local
          -> Cancellation.t @ local
          -> 'k t
          -> f:(Sync.t @ local -> 'k Capsule.Access.t -> 'a @ contended portable)
             @ local once portable unyielding
          -> 'a Or_canceled.t @ contended portable

        (** [with_read s t ~f] locks [t] for reading and runs [f], providing it shared
            access to the capsule. The [Sync.t] is passed to [f] to allow further use of
            synchronization primitives.

            If [f] raises, [t] will be frozen, meaning all subsequent attempts to acquire
            it for writing will raise. *)
        val with_read
          : 'k 'a ('b : value_or_null).
          Sync.t @ local
          -> 'k t
          -> f:(Sync.t @ local -> 'k Capsule.Access.t @ shared -> 'a @ contended portable)
             @ local once portable unyielding
          -> 'a @ contended portable

        (** [with_read_or_cancel s c t ~f] is [Completed (with_read s t ~f)] if [c] is not
            canceled, otherwise it is [Canceled]. *)
        val with_read_or_cancel
          : 'k 'a ('b : value_or_null).
          Sync.t @ local
          -> Cancellation.t @ local
          -> 'k t
          -> f:(Sync.t @ local -> 'k Capsule.Access.t @ shared -> 'a @ contended portable)
             @ local once portable unyielding
          -> 'a Or_canceled.t @ contended portable
      end

      module Expert = Sync.Rwlock
    end

    module With_rwlock : sig
      (** An ['a Capsule.With_rwlock.t] is a value of type ['a] in its own capsule,
          protected by a reader-writer lock *)
      type 'a t =
        | P :
            { data : ('a, 'k) Capsule.Data.t
            ; rwlock : 'k Rwlock.t
            }
            -> 'a t

      (** [create f] runs [f] within a fresh capsule, and creates a
          [Capsule.With_rwlock.t] containing the result *)
      val create : (unit -> 'a) @ local once portable -> 'a t

      (** [of_isolated isolated] creates a [Capsule.With_rwlock.t] from a value in an
          isolated capsule, consuming the isolated capsule. *)
      val of_isolated : 'a Capsule.Isolated.t @ unique -> 'a t

      (** [with_write s t ~f] locks the reader-writer lock associated with [t] for writing
          and calls [f] on the protected value, returning the result. The [Sync.t] is
          passed to [f] to allow further use of synchronization primitives. *)
      val with_write
        : 'a ('b : value_or_null).
        Sync.t @ local
        -> 'a t
        -> f:(Sync.t @ local -> 'a -> 'b @ contended portable)
           @ local once portable unyielding
        -> 'b @ contended portable

      (** [with_write_or_cancel s c t ~f] is [Completed (with_write s t ~f)] if [c] is not
          canceled, otherwise it is [Canceled]. *)
      val with_write_or_cancel
        : 'a ('b : value_or_null).
        Sync.t @ local
        -> Cancellation.t @ local
        -> 'a t
        -> f:(Sync.t @ local -> 'a -> 'b @ contended portable)
           @ local once portable unyielding
        -> 'b Or_canceled.t @ contended portable

      (** [with_write_guard s t ~f] locks the reader-writer lock associated with [t] for
          writing and calls [f] with the provided [sync] and a [Guard.t] for the protected
          data, returning the result.

          Since the provided callback does not have to be [portable], this is useful when
          you need to acquire two mutexes for different capsules at the same time and move
          data between them. See {!With_mutex.with_guard} for an example. *)
      val with_write_guard
        : 'a ('b : value_or_null).
        Sync.t @ local
        -> 'a t
        -> f:(Sync.t @ local -> 'a Guard.t @ local -> 'b) @ local once unyielding
        -> 'b

      (** [with_write_guard_or_cancel s c t ~f] is [Completed (with_write_guard s t ~f)]
          if [c] is not canceled, otherwise it is [Canceled]. *)
      val with_write_guard_or_cancel
        : 'a ('b : value_or_null).
        Sync.t @ local
        -> Cancellation.t @ local
        -> 'a t
        -> f:(Sync.t @ local -> 'a Guard.t @ local -> 'b) @ local once unyielding
        -> 'b Or_canceled.t

      (** [with_read s t ~f] locks the reader-writer lock associated with [t] for reading
          and calls [f] on the protected value, returning the result. The [Sync.t] is
          passed to [f] to allow further use of synchronization primitives. *)
      val with_read
        : ('a : value mod portable) ('b : value_or_null).
        Sync.t @ local
        -> 'a t
        -> f:(Sync.t @ local -> 'a @ shared -> 'b @ contended portable)
           @ local once portable unyielding
        -> 'b @ contended portable

      (** [with_read_or_cancel s c t ~f] is [Completed (with_read s t ~f)] if [c] is not
          canceled, otherwise it is [Canceled]. *)
      val with_read_or_cancel
        : ('a : value mod portable) ('b : value_or_null).
        Sync.t @ local
        -> Cancellation.t @ local
        -> 'a t
        -> f:(Sync.t @ local -> 'a @ shared -> 'b @ contended portable)
           @ local once portable unyielding
        -> 'b Or_canceled.t @ contended portable

      (** [with_read_guard s t ~f] locks the reader-writer lock associated with [t] for
          reading and calls [f] with the provided [sync] and a [Guard.Shared.t] for the
          protected data, returning the result. *)
      val with_read_guard
        : ('a : value mod portable) ('b : value_or_null).
        Sync.t @ local
        -> 'a t
        -> f:(Sync.t @ local -> 'a Guard.Shared.t @ local -> 'b) @ local once unyielding
        -> 'b

      (** [with_read_guard_or_cancel s c t ~f] is [Completed (with_read_guard s t ~f)] if
          [c] is not canceled, otherwise it is [Canceled]. *)
      val with_read_guard_or_cancel
        : ('a : value mod portable) ('b : value_or_null).
        Sync.t @ local
        -> Cancellation.t @ local
        -> 'a t
        -> f:(Sync.t @ local -> 'a Guard.Shared.t @ local -> 'b) @ local once unyielding
        -> 'b Or_canceled.t

      (** Functions that poison or freeze the lock if the provided callback raises an
          exception. *)
      module Poisoning : sig
        (** [with_write s t ~f] locks the reader-writer lock associated with [t] for
            writing and calls [f] on the protected value, returning the result. The
            [Sync.t] is passed to [f] to allow further use of synchronization primitives.

            If [f] raises, [t] will be poisoned, meaning all subsequent attempts to
            acquire it will raise. *)
        val with_write
          : 'a ('b : value_or_null).
          Sync.t @ local
          -> 'a t
          -> f:(Sync.t @ local -> 'a -> 'b @ contended portable)
             @ local once portable unyielding
          -> 'b @ contended portable

        (** [with_write_or_cancel s c t ~f] is [Completed (with_write s t ~f)] if [c] is
            not canceled, otherwise it is [Canceled].

            If [f] raises, [t] will be poisoned, meaning all subsequent attempts to
            acquire it will raise. *)
        val with_write_or_cancel
          : 'a ('b : value_or_null).
          Sync.t @ local
          -> Cancellation.t @ local
          -> 'a t
          -> f:(Sync.t @ local -> 'a -> 'b @ contended portable)
             @ local once portable unyielding
          -> 'b Or_canceled.t @ contended portable

        (** [with_write_guard s t ~f] locks the reader-writer lock associated with [t] for
            writing and calls [f] with the provided [sync] and a [Guard.t] for the
            protected data, returning the result.

            If [f] raises, [t] will be poisoned, meaning all subsequent attempts to
            acquire it will raise. *)
        val with_write_guard
          : 'a ('b : value_or_null).
          Sync.t @ local
          -> 'a t
          -> f:(Sync.t @ local -> 'a Guard.t @ local -> 'b) @ local once unyielding
          -> 'b

        (** [with_write_guard_or_cancel s c t ~f] is [Completed (with_write_guard s t ~f)]
            if [c] is not canceled, otherwise it is [Canceled].

            If [f] raises, [t] will be poisoned, meaning all subsequent attempts to
            acquire it will raise. *)
        val with_write_guard_or_cancel
          : 'a ('b : value_or_null).
          Sync.t @ local
          -> Cancellation.t @ local
          -> 'a t
          -> f:(Sync.t @ local -> 'a Guard.t @ local -> 'b) @ local once unyielding
          -> 'b Or_canceled.t

        (** [with_read s t ~f] locks the reader-writer lock associated with [t] for
            reading and calls [f] on the protected value, returning the result. The
            [Sync.t] is passed to [f] to allow further use of synchronization primitives.

            If [f] raises, [t] will be frozen, meaning all subsequent attempts to acquire
            it for writing will raise. *)
        val with_read
          : ('a : value mod portable) ('b : value_or_null).
          Sync.t @ local
          -> 'a t
          -> f:(Sync.t @ local -> 'a @ shared -> 'b @ contended portable)
             @ local once portable unyielding
          -> 'b @ contended portable

        (** [with_read_or_cancel s c t ~f] is [Completed (with_read s t ~f)] if [c] is not
            canceled, otherwise it is [Canceled].

            If [f] raises, [t] will be frozen, meaning all subsequent attempts to acquire
            it for writing will raise. *)
        val with_read_or_cancel
          : ('a : value mod portable) ('b : value_or_null).
          Sync.t @ local
          -> Cancellation.t @ local
          -> 'a t
          -> f:(Sync.t @ local -> 'a @ shared -> 'b @ contended portable)
             @ local once portable unyielding
          -> 'b Or_canceled.t @ contended portable
      end

      (** [iter_write s t ~f] is [with_write s t ~f], specialised to a function that
          returns [unit] *)
      val iter_write
        :  Sync.t @ local
        -> 'a t
        -> f:(Sync.t @ local -> 'a -> unit) @ local once portable unyielding
        -> unit

      (** [iter_read s t ~f] is [with_read s t ~f], specialised to a function that returns
          [unit] *)
      val iter_read
        : ('a : value mod portable).
        Sync.t @ local
        -> 'a t
        -> f:(Sync.t @ local -> 'a @ shared -> unit) @ local once portable unyielding
        -> unit
    end
  end

  (** Locks protecting explicit capsules which take an {!Await.t}, and may be held for an
      unbounded period of time. *)
  module Await : sig
    include module type of struct
      include Definitions (Await)
    end

    module Mutex : sig
      type 'k t = 'k Await.Mutex.t
      type packed = P : 'k t -> packed

      (** Creates a mutex with a fresh existential key type. *)
      val create : unit -> packed

      (** Like [create]. Useful in module definitions, where GADTs cannot be unpacked. *)
      module Create () : Module_with_mutex

      (** [with_lock await t ~f] acquires [t], runs [f] within the associated capsule
          (providing it {{!Capsule.Access.t} access} to the capsule), then releases [t].
          If [t] is locked, [with_lock] uses [await] to wait until it is unlocked. *)
      val with_lock
        : 'k 'a ('b : value_or_null).
        Await.t @ local
        -> 'k t
        -> f:('k Capsule.Access.t -> 'a @ contended portable) @ local once portable
        -> 'a @ contended portable

      (** [with_lock_or_cancel await c t ~f] is [Completed (with_lock await t ~f)] if [c]
          is not canceled, otherwise it is [Canceled]. *)
      val with_lock_or_cancel
        : 'k 'a ('b : value_or_null).
        Await.t @ local
        -> Cancellation.t @ local
        -> 'k t
        -> f:('k Capsule.Access.t -> 'a @ contended portable) @ local once portable
        -> 'a Or_canceled.t @ contended portable

      (** Functions that poison the mutex if the provided callback raises an exception. *)
      module Poisoning : sig
        (** [with_lock await t ~f] acquires [t], runs [f] within the associated capsule
            (providing it {{!Capsule.Access.t} access} to the capsule), then releases [t].
            If [t] is locked, [with_lock] uses [await] to wait until it is unlocked.

            If [f] raises, [t] will be poisoned, meaning all subsequent attempts to
            acquire it will raise. *)
        val with_lock
          : 'k 'a ('b : value_or_null).
          Await.t @ local
          -> 'k t
          -> f:('k Capsule.Access.t -> 'a @ contended portable) @ local once portable
          -> 'a @ contended portable

        (** [with_lock_or_cancel await c t ~f] is [Completed (with_lock await t ~f)] if
            [c] is not canceled, otherwise it is [Canceled].

            If [f] raises, [t] will be poisoned, meaning all subsequent attempts to
            acquire it will raise. *)
        val with_lock_or_cancel
          : 'k 'a ('b : value_or_null).
          Await.t @ local
          -> Cancellation.t @ local
          -> 'k t
          -> f:('k Capsule.Access.t -> 'a @ contended portable) @ local once portable
          -> 'a Or_canceled.t @ contended portable
      end

      module Expert = Await.Mutex
    end

    module With_mutex : sig
      (** An ['a Capsule.With_mutex.t] is a value of type ['a] in its own capsule,
          protected by a mutex *)
      type 'a t =
        | P :
            { data : ('a, 'k) Capsule.Data.t
            ; mutex : 'k Mutex.t
            }
            -> 'a t

      (** [create f] runs [f] within a fresh capsule, and creates a [Capsule.With_mutex.t]
          containing the result *)
      val create : (unit -> 'a) @ local once portable -> 'a t

      (** [of_isolated isolated] creates a [Capsule.With_mutex.t] from a value in an
          isolated capsule, consuming the isolated capsule. *)
      val of_isolated : 'a Capsule.Isolated.t @ unique -> 'a t

      (** [with_lock await t ~f] locks the mutex associated with [t] and calls [f] on the
          protected value, returning the result. If [t] is locked, [with_lock] uses
          [await] to wait until it is unlocked. *)
      val with_lock
        : 'a ('b : value_or_null).
        Await.t @ local
        -> 'a t
        -> f:('a -> 'b @ contended portable) @ local once portable
        -> 'b @ contended portable

      (** [with_lock_or_cancel await c t ~f] is [Completed (with_lock await t ~f)] if [c]
          is not canceled, otherwise it is [Canceled]. *)
      val with_lock_or_cancel
        : 'a ('b : value_or_null).
        Await.t @ local
        -> Cancellation.t @ local
        -> 'a t
        -> f:('a -> 'b @ contended portable) @ local once portable
        -> 'b Or_canceled.t @ contended portable

      (** [with_guard await t ~f] locks the mutex associated with [t] and calls [f] with a
          [Guard.t] for the protected data, returning the result. If [t] is locked,
          [with_guard] uses [await] to wait until it is unlocked.

          Since the provided callback does not have to be [portable], this is useful when
          you need to acquire two mutexes for different capsules at the same time and move
          data between them. For example:

          {[
            (* Lock both mutexes, and set the value of both refs to the maximum of their
               previous values, without any data races

               Make sure to always call this with locks in the same order, to avoid
               deadlocks! *)
            let set_to_max
              :  Await.t @ local -> int ref Capsule.Await.With_mutex.t
              -> int ref Capsule.Await.With_mutex.t -> unit
              =
              fun await ref1 ref2 ->
              Capsule.Await.With_mutex.with_guard await ref1 ~f:(fun guard1 ->
                Capsule.Await.With_mutex.with_guard await ref2 ~f:(fun guard2 ->
                  (* At this point we have both mutexes locked, so we can freely
                     manipulate the data they protect without the potential for data races *)
                  let value1 = Capsule.Guard.get guard1 ~f:(fun r -> !r) in
                  let value2 = Capsule.Guard.get guard2 ~f:(fun r -> !r) in
                  let new_value = Int.max value1 value2 in
                  Capsule.Guard.iter guard1 ~f:(fun r -> r := new_value);
                  Capsule.Guard.iter guard2 ~f:(fun r -> r := new_value) [@nontail])
                [@nontail])
              [@nontail]
            ;;
          ]} *)
      val with_guard
        : 'a ('b : value_or_null).
        Await.t @ local -> 'a t -> f:('a Guard.t @ local -> 'b) @ local once -> 'b

      (** [with_guard_or_cancel await c t ~f] is [Completed (with_guard await t ~f)] if
          [c] is not canceled, otherwise it is [Canceled]. *)
      val with_guard_or_cancel
        : 'a ('b : value_or_null).
        Await.t @ local
        -> Cancellation.t @ local
        -> 'a t
        -> f:('a Guard.t @ local -> 'b) @ local once
        -> 'b Or_canceled.t

      (** Functions that poison the mutex if the provided callback raises an exception. *)
      module Poisoning : sig
        (** [with_lock await t ~f] locks the mutex associated with [t] and calls [f] on
            the protected value, returning the result. If [t] is locked, [with_lock] uses
            [await] to wait until it is unlocked.

            If [f] raises, [t] will be poisoned, meaning all subsequent attempts to
            acquire it will raise. *)
        val with_lock
          : 'a ('b : value_or_null).
          Await.t @ local
          -> 'a t
          -> f:('a -> 'b @ contended portable) @ local once portable
          -> 'b @ contended portable

        (** [with_lock_or_cancel await c t ~f] is [Completed (with_lock await t ~f)] if
            [c] is not canceled, otherwise it is [Canceled]. *)
        val with_lock_or_cancel
          : 'a ('b : value_or_null).
          Await.t @ local
          -> Cancellation.t @ local
          -> 'a t
          -> f:('a -> 'b @ contended portable) @ local once portable
          -> 'b Or_canceled.t @ contended portable
      end

      (** [iter await t ~f] is [with_lock await t ~f], specialised to a function that
          returns [unit] *)
      val iter : Await.t @ local -> 'a t -> f:('a -> unit) @ local once portable -> unit

      (** [map await t ~f] locks the mutex associated with [t] and calls [f] on the
          protected value, returning a new [With_mutex.t] containing the result in the
          same capsule and protected by the same mutex. *)
      val map : Await.t @ local -> 'a t -> f:('a -> 'b) @ local once portable -> 'b t

      (** [destroy await t] acquires and then poisons the mutex associated with [t],
          merging the protected value into the current capsule and returning it. *)
      val destroy : Await.t @ local -> 'a t -> 'a
    end

    module Rwlock : sig
      type 'k t = 'k Await.Rwlock.t
      type packed = P : 'k t -> packed

      (** Creates a reader-writer lock with a fresh existential key type. *)
      val create : unit -> packed

      (** Like [create]. Useful in module definitions, where GADTs cannot be unpacked. *)
      module Create () : Module_with_rwlock

      (** [with_write await t ~f] acquires [t] for writing, runs [f] within the associated
          capsule (providing it {{!Capsule.Access.t} access} to the capsule), then
          releases [t]. If [t] is locked, [with_write] uses [await] to wait until it is
          unlocked. *)
      val with_write
        : 'k 'a ('b : value_or_null).
        Await.t @ local
        -> 'k t
        -> f:('k Capsule.Access.t -> 'a @ contended portable) @ local once portable
        -> 'a @ contended portable

      (** [with_write_or_cancel await c t ~f] is [Completed (with_write await t ~f)] if
          [c] is not canceled, otherwise it is [Canceled]. *)
      val with_write_or_cancel
        : 'k 'a ('b : value_or_null).
        Await.t @ local
        -> Cancellation.t @ local
        -> 'k t
        -> f:('k Capsule.Access.t -> 'a @ contended portable) @ local once portable
        -> 'a Or_canceled.t @ contended portable

      (** [with_read await t ~f] acquires [t] for reading, runs [f] with shared
          {{!Capsule.Access.t} access} to the associated capsule, then releases [t]. If
          [t] is locked for writing, [with_read] uses [await] to wait until it is
          unlocked. *)
      val with_read
        : 'k 'a ('b : value_or_null).
        Await.t @ local
        -> 'k t
        -> f:('k Capsule.Access.t @ shared -> 'a @ contended portable)
           @ local once portable
        -> 'a @ contended portable

      (** [with_read_or_cancel await c t ~f] is [Completed (with_read await t ~f)] if [c]
          is not canceled, otherwise it is [Canceled]. *)
      val with_read_or_cancel
        : 'k 'a ('b : value_or_null).
        Await.t @ local
        -> Cancellation.t @ local
        -> 'k t
        -> f:('k Capsule.Access.t @ shared -> 'a @ contended portable)
           @ local once portable
        -> 'a Or_canceled.t @ contended portable

      (** Functions that poison or freeze the lock if the provided callback raises an
          exception. *)
      module Poisoning : sig
        (** [with_write await t ~f] acquires [t] for writing, runs [f] within the
            associated capsule (providing it {{!Capsule.Access.t} access} to the capsule),
            then releases [t]. If [t] is locked, [with_write] uses [await] to wait until
            it is unlocked.

            If [f] raises, [t] will be poisoned, meaning all subsequent attempts to
            acquire it will raise. *)
        val with_write
          : 'k 'a ('b : value_or_null).
          Await.t @ local
          -> 'k t
          -> f:('k Capsule.Access.t -> 'a @ contended portable) @ local once portable
          -> 'a @ contended portable

        (** [with_write_or_cancel await c t ~f] is [Completed (with_write await t ~f)] if
            [c] is not canceled, otherwise it is [Canceled]. *)
        val with_write_or_cancel
          : 'k 'a ('b : value_or_null).
          Await.t @ local
          -> Cancellation.t @ local
          -> 'k t
          -> f:('k Capsule.Access.t -> 'a @ contended portable) @ local once portable
          -> 'a Or_canceled.t @ contended portable

        (** [with_read await t ~f] acquires [t] for reading, runs [f] with shared
            {{!Capsule.Access.t} access} to the associated capsule, then releases [t]. If
            [t] is locked for writing, [with_read] uses [await] to wait until it is
            unlocked.

            If [f] raises, [t] will be frozen, meaning all subsequent attempts to acquire
            it for writing will raise. *)
        val with_read
          : 'k 'a ('b : value_or_null).
          Await.t @ local
          -> 'k t
          -> f:('k Capsule.Access.t @ shared -> 'a @ contended portable)
             @ local once portable
          -> 'a @ contended portable

        (** [with_read_or_cancel await c t ~f] is [Completed (with_read await t ~f)] if
            [c] is not canceled, otherwise it is [Canceled]. *)
        val with_read_or_cancel
          : 'k 'a ('b : value_or_null).
          Await.t @ local
          -> Cancellation.t @ local
          -> 'k t
          -> f:('k Capsule.Access.t @ shared -> 'a @ contended portable)
             @ local once portable
          -> 'a Or_canceled.t @ contended portable
      end

      module Expert = Await.Rwlock
    end

    module With_rwlock : sig
      (** An ['a Capsule.With_rwlock.t] is a value of type ['a] in its own capsule,
          protected by a reader-writer lock *)
      type 'a t =
        | P :
            { data : ('a, 'k) Capsule.Data.t
            ; rwlock : 'k Rwlock.t
            }
            -> 'a t

      (** [create f] runs [f] within a fresh capsule, and creates a
          [Capsule.With_rwlock.t] containing the result *)
      val create : (unit -> 'a) @ local once portable -> 'a t

      (** [of_isolated isolated] creates a [Capsule.With_rwlock.t] from a value in an
          isolated capsule, consuming the isolated capsule. *)
      val of_isolated : 'a Capsule.Isolated.t @ unique -> 'a t

      (** [with_write await t ~f] locks the reader-writer lock associated with [t] for
          writing and calls [f] on the protected value, returning the result. If [t] is
          locked, [with_write] uses [await] to wait until it is unlocked. *)
      val with_write
        : 'a ('b : value_or_null).
        Await.t @ local
        -> 'a t
        -> f:('a -> 'b @ contended portable) @ local once portable
        -> 'b @ contended portable

      (** [with_write_or_cancel await c t ~f] is [Completed (with_write await t ~f)] if
          [c] is not canceled, otherwise it is [Canceled]. *)
      val with_write_or_cancel
        : 'a ('b : value_or_null).
        Await.t @ local
        -> Cancellation.t @ local
        -> 'a t
        -> f:('a -> 'b @ contended portable) @ local once portable
        -> 'b Or_canceled.t @ contended portable

      (** [with_write_guard await t ~f] locks the reader-writer lock associated with [t]
          for writing and calls [f] with a [Guard.t] for the protected data, returning the
          result. If [t] is locked, [with_write_guard] uses [await] to wait until it is
          unlocked.

          Since the provided callback does not have to be [portable], this is useful when
          you need to acquire two mutexes for different capsules at the same time and move
          data between them. See {!With_mutex.with_guard} for an example. *)
      val with_write_guard
        : 'a ('b : value_or_null).
        Await.t @ local -> 'a t -> f:('a Guard.t @ local -> 'b) @ local once -> 'b

      (** [with_write_guard_or_cancel await c t ~f] is
          [Completed (with_write_guard await t ~f)] if [c] is not canceled, otherwise it
          is [Canceled]. *)
      val with_write_guard_or_cancel
        : 'a ('b : value_or_null).
        Await.t @ local
        -> Cancellation.t @ local
        -> 'a t
        -> f:('a Guard.t @ local -> 'b) @ local once
        -> 'b Or_canceled.t

      (** [with_read await t ~f] locks the reader-writer lock associated with [t] for
          reading and calls [f] on the protected value, returning the result. If [t] is
          locked for writing, [with_read] uses [await] to wait until it is unlocked. *)
      val with_read
        : ('a : value mod portable) ('b : value_or_null).
        Await.t @ local
        -> 'a t
        -> f:('a @ shared -> 'b @ contended portable) @ local once portable
        -> 'b @ contended portable

      (** [with_read_or_cancel await c t ~f] is [Completed (with_read await t ~f)] if [c]
          is not canceled, otherwise it is [Canceled]. *)
      val with_read_or_cancel
        : ('a : value mod portable) ('b : value_or_null).
        Await.t @ local
        -> Cancellation.t @ local
        -> 'a t
        -> f:('a @ shared -> 'b @ contended portable) @ local once portable
        -> 'b Or_canceled.t @ contended portable

      (** [with_read_guard await t ~f] locks the reader-writer lock associated with [t]
          for reading and calls [f] with a [Guard.Shared.t] for the protected data,
          returning the result. If [t] is locked for writing, [with_read_guard] uses
          [await] to wait until it is unlocked. *)
      val with_read_guard
        : ('a : value mod portable) ('b : value_or_null).
        Await.t @ local -> 'a t -> f:('a Guard.Shared.t @ local -> 'b) @ local once -> 'b

      (** [with_read_guard_or_cancel await c t ~f] is
          [Completed (with_read_guard await t ~f)] if [c] is not canceled, otherwise it is
          [Canceled]. *)
      val with_read_guard_or_cancel
        : ('a : value mod portable) ('b : value_or_null).
        Await.t @ local
        -> Cancellation.t @ local
        -> 'a t
        -> f:('a Guard.Shared.t @ local -> 'b) @ local once
        -> 'b Or_canceled.t

      (** Functions that poison or freeze the lock if the provided callback raises an
          exception. *)
      module Poisoning : sig
        (** [with_write await t ~f] locks the reader-writer lock associated with [t] for
            writing and calls [f] on the protected value, returning the result. If [t] is
            locked, [with_write] uses [await] to wait until it is unlocked.

            If [f] raises, [t] will be poisoned, meaning all subsequent attempts to
            acquire it will raise. *)
        val with_write
          : 'a ('b : value_or_null).
          Await.t @ local
          -> 'a t
          -> f:('a -> 'b @ contended portable) @ local once portable
          -> 'b @ contended portable

        (** [with_write_or_cancel await c t ~f] is [Completed (with_write await t ~f)] if
            [c] is not canceled, otherwise it is [Canceled]. *)
        val with_write_or_cancel
          : 'a ('b : value_or_null).
          Await.t @ local
          -> Cancellation.t @ local
          -> 'a t
          -> f:('a -> 'b @ contended portable) @ local once portable
          -> 'b Or_canceled.t @ contended portable

        (** [with_write_guard await t ~f] locks the reader-writer lock associated with [t]
            for writing and calls [f] with a [Guard.t] for the protected value, returning
            the result. If [t] is locked, [with_write_guard] uses [await] to wait until it
            is unlocked.

            If [f] raises, [t] will be poisoned, meaning all subsequent attempts to
            acquire it will raise. *)
        val with_write_guard
          : 'a ('b : value_or_null).
          Await.t @ local -> 'a t -> f:('a Guard.t @ local -> 'b) @ local once -> 'b

        (** [with_write_guard_or_cancel await c t ~f] is
            [Completed (with_write_guard await t ~f)] if [c] is not canceled, or
            [Canceled] otherwise. *)
        val with_write_guard_or_cancel
          : 'a ('b : value_or_null).
          Await.t @ local
          -> Cancellation.t @ local
          -> 'a t
          -> f:('a Guard.t @ local -> 'b) @ local once
          -> 'b Or_canceled.t

        (** [with_read await t ~f] locks the reader-writer lock associated with [t] for
            reading and calls [f] on the protected value, returning the result. If [t] is
            locked for writing, [with_read] uses [await] to wait until it is unlocked.

            If [f] raises, [t] will be frozen, meaning all subsequent attempts to acquire
            it for writing will raise. *)
        val with_read
          : ('a : value mod portable) ('b : value_or_null).
          Await.t @ local
          -> 'a t
          -> f:('a @ shared -> 'b @ contended portable) @ local once portable
          -> 'b @ contended portable

        (** [with_read_or_cancel await c t ~f] is [Completed (with_read await t ~f)] if
            [c] is not canceled, otherwise it is [Canceled]. *)
        val with_read_or_cancel
          : ('a : value mod portable) ('b : value_or_null).
          Await.t @ local
          -> Cancellation.t @ local
          -> 'a t
          -> f:('a @ shared -> 'b @ contended portable) @ local once portable
          -> 'b Or_canceled.t @ contended portable

        (** [with_read_guard await t ~f] locks the reader-writer lock associated with [t]
            for reading and calls [f] with a [Guard.Shared.t] for the protected data,
            returning the result. If [t] is locked for writing, [with_read_guard] uses
            [await] to wait until it is unlocked.

            If [f] raises, [t] will be frozen, meaning all subsequent attempts to acquire
            it for writing will raise. *)
        val with_read_guard
          : ('a : value mod portable) ('b : value_or_null).
          Await.t @ local
          -> 'a t
          -> f:('a Guard.Shared.t @ local -> 'b) @ local once
          -> 'b

        (** [with_read_guard_or_cancel await c t ~f] is
            [Completed (with_read_guard await t ~f)] if [c] is not canceled, otherwise it
            is [Canceled]. *)
        val with_read_guard_or_cancel
          : ('a : value mod portable) ('b : value_or_null).
          Await.t @ local
          -> Cancellation.t @ local
          -> 'a t
          -> f:('a Guard.Shared.t @ local -> 'b) @ local once
          -> 'b Or_canceled.t
      end

      (** [iter_write await t ~f] is [with_write await t ~f], specialised to a function
          that returns [unit]. *)
      val iter_write
        :  Await.t @ local
        -> 'a t
        -> f:('a -> unit) @ local once portable
        -> unit

      (** [iter_read await t ~f] is [with_read await t ~f], specialised to a function that
          returns [unit]. *)
      val iter_read
        : ('a : value mod portable).
        Await.t @ local -> 'a t -> f:('a @ shared -> unit) @ local once portable -> unit
    end
  end
end
