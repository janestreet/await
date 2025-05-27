open! Base
open Portable
open Expect_test_helpers_base
open Await_test_helpers

let%expect_test "current_domain (from initial domain)" =
  let initial = Multicore.current_domain () in
  print_s [%sexp (initial : int)];
  [%expect {| 0 |}]
;;

let%expect_test ("current_domain (from non-initial domain)" [@tags "runtime5-only"]) =
  let non_initial = Atomic.make None in
  Multicore.spawn_on ~domain:1 (fun () ->
    Atomic.set non_initial (Some (Multicore.current_domain ())));
  while Option.is_none (Atomic.get non_initial) do
    Thread.yield ()
  done;
  print_s [%sexp (Atomic.get non_initial : int option)];
  [%expect {| (1) |}]
;;
