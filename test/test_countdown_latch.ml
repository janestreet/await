open! Core
open Await
open Expect_test_helpers_core

let%expect_test "sexp_of_t" =
  let latch = Countdown_latch.create 3 in
  print_s [%sexp (latch : Countdown_latch.t)];
  [%expect {| (count 3) |}];
  Countdown_latch.decr latch;
  print_s [%sexp (latch : Countdown_latch.t)];
  [%expect {| (count 2) |}]
;;

let%expect_test "get_count" =
  let latch = Countdown_latch.create 3 in
  print_s [%sexp (Countdown_latch.count latch : int)];
  [%expect {| 3 |}];
  Countdown_latch.decr latch;
  print_s [%sexp (Countdown_latch.count latch : int)];
  [%expect {| 2 |}]
;;

let%expect_test "decr too many times" =
  let latch = Countdown_latch.create 3 in
  require_does_raise (fun () ->
    for i = 0 to 3 do
      Countdown_latch.decr latch;
      print_s [%message "decr" (i : int)]
    done);
  [%expect
    {|
    (decr (i 0))
    (decr (i 1))
    (decr (i 2))
    (Invalid_argument "Countdown_latch.decr: already reached zero")
    |}]
;;

let%expect_test "create with negative count" =
  require_does_raise (fun () -> Countdown_latch.create (-1));
  [%expect {| (Invalid_argument "Countdown_latch.create: invalid initial count") |}]
;;

let%expect_test "await" =
  let latch = Countdown_latch.create 3 in
  let%with.tilde.stack conc = Concurrent_in_thread.with_concurrent Terminator.never in
  Concurrent.with_scope conc () ~f:(fun s ->
    Concurrent.spawn s ~f:(fun _ _ conc ->
      Countdown_latch.await (Concurrent.await conc) latch;
      print_endline "await finished!");
    for _ = 0 to 2 do
      print_s [%message "counting down" (latch : Countdown_latch.t)];
      Countdown_latch.decr latch
    done);
  [%expect
    {|
    ("counting down" (latch (count 3)))
    ("counting down" (latch (count 2)))
    ("counting down" (latch (count 1)))
    await finished!
    |}]
;;

let%expect_test "poison" =
  let latch = Countdown_latch.create 3 in
  print_s [%sexp (Countdown_latch.is_poisoned latch : bool)];
  [%expect {| false |}];
  Countdown_latch.poison latch;
  print_s [%sexp (Countdown_latch.is_poisoned latch : bool)];
  [%expect {| true |}];
  print_s [%sexp (latch : Countdown_latch.t)];
  [%expect {| (poisoned 3) |}]
;;

let%expect_test "decr on poisoned latch" =
  let latch = Countdown_latch.create 3 in
  Countdown_latch.poison latch;
  Countdown_latch.decr latch;
  print_s [%sexp (latch : Countdown_latch.t)];
  [%expect {| (poisoned 2) |}]
;;

let%expect_test "incr" =
  let latch = Countdown_latch.create 3 in
  print_s [%sexp (Countdown_latch.count latch : int)];
  [%expect {| 3 |}];
  Countdown_latch.incr latch;
  print_s [%sexp (Countdown_latch.count latch : int)];
  [%expect {| 4 |}];
  Countdown_latch.incr latch;
  print_s [%sexp (Countdown_latch.count latch : int)];
  [%expect {| 5 |}]
;;

let%expect_test "incr on poisoned latch" =
  let latch = Countdown_latch.create 3 in
  Countdown_latch.poison latch;
  require_does_raise (fun () -> Countdown_latch.incr latch);
  [%expect {| (Poisoned) |}]
;;

let%expect_test "incr after reaching zero" =
  let latch = Countdown_latch.create 2 in
  Countdown_latch.decr latch;
  Countdown_latch.decr latch;
  print_s [%sexp (Countdown_latch.count latch : int)];
  [%expect {| 0 |}];
  require_does_raise (fun () -> Countdown_latch.incr latch);
  [%expect {| (Invalid_argument "Countdown_latch.decr: already reached zero") |}]
;;

let%expect_test "await on poisoned latch" =
  let latch = Countdown_latch.create 3 in
  Countdown_latch.poison latch;
  let%with.tilde.stack conc = Concurrent_in_thread.with_concurrent Terminator.never in
  let aw = Concurrent.await conc in
  require_does_raise (fun () -> Countdown_latch.await aw latch);
  [%expect {| (Poisoned) |}]
;;

let%expect_test "poison while waiting" =
  let latch = Countdown_latch.create 2 in
  let%with.tilde.stack conc = Concurrent_in_thread.with_concurrent Terminator.never in
  Concurrent.with_scope conc () ~f:(fun s ->
    Concurrent.spawn s ~f:(fun _ _ conc ->
      let aw = Concurrent.await conc in
      require_does_raise (fun () -> Countdown_latch.await aw latch) [@nontail]);
    Countdown_latch.decr latch;
    print_s [%message "after first decr" (latch : Countdown_latch.t)];
    Countdown_latch.poison latch;
    print_s [%message "after poison" (latch : Countdown_latch.t)]);
  [%expect
    {|
    ("after first decr" (latch (count 1)))
    ("after poison" (latch (poisoned 1)))
    (Poisoned)
    |}]
;;
