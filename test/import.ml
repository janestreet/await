open Base
open Await

module type Empty = sig end

module type S = sig
  type context

  val scalable : bool
  val with_concurrent : f:(context Concurrent.t @ local -> unit) @ portable -> unit
  val with_await : f:(Await.t @ local -> unit) @ portable -> unit
end

module Test_with_all_schedulers (Test_with_scheduler : functor (_ : S) -> Empty) = struct
  module Test_in_thread = Test_with_scheduler (struct
      type context = unit

      let scalable = false
      let with_concurrent ~f = Concurrent_in_thread.with_concurrent Terminator.never ~f
      let with_await ~f = Await_blocking.with_await Terminator.never ~f
    end)

  module Test_parallel = Test_with_scheduler (struct
      type context = Parallel.t

      let scalable = Multicore.max_domains () > 1

      let scheduler =
        (Parallel_scheduler_work_stealing.create [@alert "-experimental"]) ()
      ;;

      let with_concurrent ~f =
        Parallel_scheduler_work_stealing.concurrent
          scheduler
          ~terminator:Terminator.never
          ~f
      ;;

      let with_await ~f =
        with_concurrent ~f:(fun concurrent -> f (Concurrent.await concurrent) [@nontail])
      ;;
    end)
end
