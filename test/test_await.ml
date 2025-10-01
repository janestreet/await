open Core
open Await
open Expect_test_helpers_core

let%expect_test "[Await.never]" =
  let%with.tilde.stack w = Await.For_testing.with_never in
  let trigger = Trigger.create () in
  require_does_raise (fun () ->
    Await.await w ~on_terminate:(Trigger.source trigger) ~await_on:trigger);
  [%expect
    {|
    (Failure
     "[await never] was called. Usually this means that an operation blocked which was expected to never block")
    |}]
;;

let%expect_test "[Await.with_] doesn't allocate" =
  let await () _ = failwith "await called" in
  require_no_allocation (fun () ->
    Await.with_ ~terminator:Terminator.never ~await ~yield:Null () ~f:(fun _ -> ()))
;;

let%expect_test "[Await_spinning.with_await] doesn't allocate" =
  require_no_allocation (fun () ->
    Await_spinning.with_await Terminator.never ~f:(fun _ -> ()))
;;
