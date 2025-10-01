open Core
open Import
open Await
open Expect_test_helpers_core

let%expect_test "[Await_blocking.with_await] doesn't allocate if nothing ever blocks" =
  let f (_ : Await.t) = () in
  require_no_allocation (fun () -> Await_blocking.with_await Terminator.never ~f);
  [%expect {| |}]
;;

module Test (S : S) = struct
  let%expect_test "Await_blocking basics" =
    (* This tests that we can create parallel threads and read/await [Ivar]s in them using
     the awaiter provided by [with_await]. *)
    let ivar0 : int Ivar.t = Ivar.create () in
    let ivar1 : int Ivar.t = Ivar.create () in
    let ivar2 : int Ivar.t = Ivar.create () in
    let ivar3 : int Ivar.t = Ivar.create () in
    S.with_concurrent ~f:(fun c ->
      Concurrent.with_scope c () ~f:(fun s ->
        Concurrent.spawn s ~f:(fun _ _ c ->
          (match Ivar.read_or_cancel (Concurrent.await c) Cancellation.never ivar0 with
           | Canceled -> print_endline "Error: Canceled"
           | Completed value -> require_equal (module Int) 42 value);
          print_endline "signal 1";
          Ivar.fill_exn ivar1 101;
          require_equal (module Int) 76 (Ivar.read (Concurrent.await c) ivar2));
        Concurrent.spawn s ~f:(fun _ _ c ->
          require_equal (module Int) 19 (Ivar.read (Concurrent.await c) ivar3);
          print_endline "signal 0";
          Ivar.fill_exn ivar0 42;
          require_equal (module Int) 101 (Ivar.read (Concurrent.await c) ivar1));
        Concurrent.spawn s ~f:(fun _ _ _ ->
          print_endline "start";
          Ivar.fill_exn ivar2 76;
          Ivar.fill_exn ivar3 19)));
    [%expect
      {|
      start
      signal 0
      signal 1
      |}]
  ;;
end

module%test _ = Test_with_all_schedulers (Test)
