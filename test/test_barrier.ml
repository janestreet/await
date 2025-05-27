open Core
open Await
open Await_sync
open Await_test_helpers

let spawn = Multicore.spawn
and setup = Await_blocking.with_await

let%expect_test "Barrier basics" =
  setup Terminator.never ~f:(fun w ->
    for n = 1 to 4 do
      Stdio.printf "%d %!" n;
      let barrier = Barrier.create n in
      Structured.with_scope w () ~f:(fun _w s ->
        let n_outside = Atomic.make n in
        for _ = 1 to n do
          Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun w () ->
            for _ = 1 to 5 do
              Atomic.decr n_outside;
              Barrier.await w barrier;
              let n = Atomic.get n_outside in
              if n <> 0 then Stdio.printf "Error: %d%!" n;
              Barrier.await w barrier;
              Atomic.incr n_outside
            done)
        done)
    done);
  [%expect {| 1 2 3 4 |}]
;;

let%expect_test "Barrier poisoning" =
  setup Terminator.never ~f:(fun w ->
    for n = 2 to 5 do
      match
        let barrier = Barrier.create n in
        Structured.with_scope w () ~f:(fun _w s ->
          for _ = 2 to n do
            Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun w () ->
              Barrier.await w barrier;
              Stdio.print_endline "Error: Barrier.await returned")
          done;
          Barrier.poison barrier)
      with
      | exception Barrier.Poisoned -> Stdio.printf "%d %!" n
      | () -> Stdio.print_endline "Error: Model.with_scope returned"
    done);
  [%expect {| 2 3 4 5 |}]
;;

let%expect_test "More barrier poisoning" =
  setup Terminator.never ~f:(fun w ->
    for n = 3 to 6 do
      match
        let barrier = Barrier.create n in
        Structured.with_scope w () ~f:(fun w s ->
          Terminator.with_linked (terminator w) (fun to_be_terminated ->
            let to_be_terminated = Terminator.Expert.globalize to_be_terminated in
            Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun w () ->
              Barrier.await (Await.with_terminator w to_be_terminated) barrier;
              Stdio.print_endline "Error: Barrier.await returned");
            for _ = 3 to n do
              Structured.Scope.(add [@mode portable local])
                ~spawn
                ~setup
                s
                ~f:(fun w () ->
                  Barrier.await w barrier;
                  Stdio.print_endline "Error: Barrier.await returned")
            done;
            Option.iter
              ~f:Terminator.Source.terminate
              (Terminator.source to_be_terminated))
          [@nontail])
      with
      | exception Barrier.Poisoned -> Stdio.printf "%d %!" n
      | () -> Stdio.print_endline "Error: returned normally"
      | exception _ -> Stdio.print_endline "Error: raised unexpected"
    done);
  [%expect {| 3 4 5 6 |}]
;;
