open Base
open Async
open Await_kernel

(** [schedule_with_await terminator ~f] schedules [f w] as a fiber to run on the async
    scheduler with a [w : Await.t] that will resume execution of the fiber after a
    blocking await on the async scheduler. *)
val schedule_with_await
  :  ?monitor:Monitor.t
  -> ?priority:Priority.t
  -> Terminator.t
  -> f:(Await.t @ local -> 'a) @ once
  -> 'a Deferred.t
[@@alert
  experimental
    "Effects are not supported in the OCaml 4 runtime, and breaking runtime4 builds does \
     not block continuous release. Please refrain from using effects until the 4 runtime \
     has been deprecated."]

(** [await_deferred w deferred] awaits until the deferred becomes determined.

    @raise Terminated if [w] is terminated before [deferred] becomes determined. *)
val await_deferred : Await.t @ local -> 'a Deferred.t -> 'a
[@@alert
  experimental
    "Effects are not supported in the OCaml 4 runtime, and breaking runtime4 builds does \
     not block continuous release. Please refrain from using effects until the 4 runtime \
     has been deprecated."]

module Expert : sig
  (** [with_await terminator ~f] runs [f w] as a fiber with a [w : Await.t] that will
      resume execution of the fiber after a blocking await on the async scheduler.
      [with_await] may return as soon as the first blocking await in [f w] is encountered. *)
  val with_await : Terminator.t @ local -> f:(Await.t @ local -> unit) @ once -> unit
  [@@alert
    experimental
      "Effects are not supported in the OCaml 4 runtime, and breaking runtime4 builds \
       does not block continuous release. Please refrain from using effects until the 4 \
       runtime has been deprecated."]

  (** [with_yield ~f] runs [f w] with a [w : Yield.t] that will yield to the async
      scheduler . *)
  val with_yield : f:(Yield.t @ local -> unit) @ once -> unit
  [@@alert
    experimental
      "Effects are not supported in the OCaml 4 runtime, and breaking runtime4 builds \
       does not block continuous release. Please refrain from using effects until the 4 \
       runtime has been deprecated."]

  (** [thread_safe_spawn execution_context f] is
      [Async_kernel_scheduler.thread_safe_enqueue_job execution_context f ()]. *)
  val thread_safe_spawn : Execution_context.t -> (unit -> unit) @ once -> unit
end
