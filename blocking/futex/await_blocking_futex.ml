(** A handle to a linux futex for waiting on a trigger. *)
type t : immediate

type count : immediate

(** Returns a futex that may be used to wait on a trigger. A new futex may or may not be
    returned by each call to [get]. *)
external get : unit -> t @@ portable = "await_blocking_futex_get"
[@@noalloc]

(** Returns the current count of the futex. *)
external count : t -> count @@ portable = "await_blocking_futex_count"
[@@noalloc]

(** Increments the count of the futex. Any call to [wait] on the same futex will check
    whether the associated trigger has been signaled before suspending the thread. *)
external signal : t -> unit @@ portable = "await_blocking_futex_signal"
[@@noalloc]

(** Wait until the count of the futex changes by suspending the thread until the futex is
    [signal]ed, then return the current count. *)
external wait : t -> count:count -> count @@ portable = "await_blocking_futex_wait"
