@@ portable

(** Structured concurrency with {!Await}. *)

open Await

module (Scope @ nonportable) : sig
  (** Represents a scope for concurrency. *)
  type 'a t : value mod contended portable

  (** [add ~spawn ~setup s ~f] [spawn]s a new thread of control into the scope [s] to run
      [f w c], where [w] is given by [setup] and [c] is the context of the scope.

      If [f] raises an exception other than [Terminated], the scope will be terminated. *)
  val%template add
    :  spawn:((unit -> unit) @ once portability -> unit) @ once portability
    -> setup:(Terminator.t @ local -> f:('w @ local -> unit) @ locality once -> unit)
       @ once portability
    -> 'a t @ local
    -> f:('w @ local -> 'a @ contended local -> unit) @ once portability
    -> unit
    @@ portability
  [@@mode portability = (nonportable, portable), locality = (global, local)]
end

(** [with_scope w c ~f] is [f w' s] where [s] is a local {!Scope} into which concurrent
    threads of control can be spawned into and [w'] is [w] whose terminator has been
    replaced with the new terminator of [s] linked to the terminator of [w].

    The [c] context argument argument is passed in [@ contended local] to each thread, to
    allow threading local context into spawned threads.

    If [f] raises an exception other than [Terminated], the scope will be terminated.

    [with_scope] does not return until all of the threads of control spawned into the
    scope have terminated. If any thread added to the scope has raised an exception other
    than [Terminated] or if [f] raises an exception other than [Terminated], then
    [with_scope] will raise the exception that was caught by the scope first. *)
val with_scope
  :  Await.t @ local
  -> 'a @ portable
  -> f:(Await.t @ local -> 'a Scope.t @ local -> 'b) @ local once
  -> 'b
