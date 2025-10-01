open Core
open Import
open Await
open Expect_test_helpers_base

let print t = print_s [%sexp (t : int Stack.t)]

let%expect_test "single-threaded operation" =
  let t = Stack.create () in
  Await_blocking.with_await Terminator.never ~f:(fun w ->
    print t;
    [%expect {| () |}];
    Stack.push t 1;
    print t;
    [%expect {| (1) |}];
    Stack.push t 2;
    print t;
    [%expect {| (2 1) |}];
    Stack.push t 3;
    print t;
    [%expect {| (3 2 1) |}];
    Stack.pop w t |> [%sexp_of: int] |> print_s;
    [%expect {| 3 |}];
    Stack.pop w t |> [%sexp_of: int] |> print_s;
    [%expect {| 2 |}];
    Stack.pop w t |> [%sexp_of: int] |> print_s;
    [%expect {| 1 |}];
    Stack.push t 1;
    print t;
    [%expect {| (1) |}];
    print_s [%sexp (Stack.pop_nonblocking t : int or_null)];
    [%expect {| (1) |}];
    print t;
    [%expect {| () |}];
    print_s [%sexp (Stack.pop_nonblocking t : int or_null)];
    [%expect {| () |}];
    Stack.push t 1;
    Stack.push t 2;
    Stack.push t 3;
    print_s [%sexp (Stack.drain t : int list)];
    [%expect {| (3 2 1) |}])
;;

module Test (S : S) = struct
  let%expect_test "block on an empty queue" =
    S.with_concurrent ~f:(fun c ->
      let t = Stack.create () in
      Concurrent.with_scope c () ~f:(fun s ->
        let barrier = Barrier.create 2 in
        Concurrent.spawn s ~f:(fun _ _ c ->
          Barrier.await (Concurrent.await c) barrier;
          let res : int = Stack.pop (Concurrent.await c) t in
          Barrier.await (Concurrent.await c) barrier;
          printf "domain popped %d%!" res);
        Barrier.await (Concurrent.await c) barrier;
        Stack.push t 1;
        Barrier.await (Concurrent.await c) barrier [@nontail])
      [@nontail]);
    [%expect {| domain popped 1 |}]
  ;;

  let%expect_test "cancel a blocking pop" =
    S.with_concurrent ~f:(fun c ->
      let t = Stack.create () in
      let result = Atomic.make None in
      Cancellation.with_ (fun cancellation ->
        Concurrent.with_scope c (Cancellation.Expert.globalize cancellation) ~f:(fun s ->
          Concurrent.spawn s ~f:(fun s _ c ->
            let res = Stack.pop_or_cancel (Concurrent.await c) (Scope.context s) t in
            Atomic.set result (Some res));
          Concurrent.spawn s ~f:(fun s _ _ ->
            Scope.context s
            |> Cancellation.source
            |> Or_null.value_exn
            |> Cancellation.Source.cancel)));
      print_s [%sexp (result : int Or_canceled.t option Atomic.t)]);
    [%expect {| (Canceled) |}]
  ;;

  module Operation = struct
    type t =
      | Push of int
      | Pop
      | Drain
      | Pop_nonblocking [@quickcheck.weight 1. /. 5.]
    [@@deriving quickcheck, sexp_of]

    let run_or_cancel await c stack t =
      let open Or_canceled.Let_syntax in
      match t with
      | Push i ->
        let%map () = Cancellation.check c in
        Stack.push stack i;
        []
      | Pop -> Stack.pop_or_cancel await c stack >>| List.singleton
      | Drain ->
        let%map () = Cancellation.check c in
        Stack.drain stack
      | Pop_nonblocking ->
        let%map () = Cancellation.check c in
        Stack.pop_nonblocking stack |> Or_null.to_list
    ;;
  end

  let generate_ops =
    let open Quickcheck.Generator.Let_syntax in
    let%bind len = Quickcheck.Generator.small_positive_int in
    let operations =
      let%bind len = Quickcheck.Generator.small_positive_int in
      List.gen_with_length (len mod 20) [%quickcheck.generator: Operation.t]
    in
    List.gen_with_length (len mod 8) operations
  ;;

  let%quick_test ("many operations in parallel" [@trials 3]) =
    fun (ops : (Operation.t list list[@generator generate_ops])) ->
    let timeout_sec = 10 in
    let%with.tilde await = S.with_concurrent in
    let%with.stack c = Cancellation.with_ in
    let t = Stack.create () in
    let results = Atomic.make [] in
    let expected_results = Atomic.make [] in
    let timed_out = Atomic.make false in
    Concurrent.with_scope await (Cancellation.Expert.globalize c) ~f:(fun s ->
      let barrier = Barrier.create (List.length ops + 1) in
      let running = Atomic.make 0 in
      (* Run a task to time out after a period of time *)
      Concurrent.spawn s ~f:(fun s _ _ ->
        Core_unix.sleep timeout_sec;
        Cancellation.Source.cancel
          (s |> Scope.context |> Cancellation.source |> Or_null.value_exn);
        if Atomic.get running > 0 then Atomic.set timed_out true);
      List.iter ops ~f:(fun ops ->
        Concurrent.spawn s ~f:(fun s _ conc ->
          Atomic.incr running;
          Barrier.await_or_cancel (Concurrent.await conc) (Scope.context s) barrier
          |> Or_canceled.completed_exn;
          let[@inline] rec go : _ -> _ Or_canceled.t = function
            | op :: ops ->
              (match
                 Operation.run_or_cancel (Concurrent.await conc) (Scope.context s) t op
               with
               | Completed op_results ->
                 Atomic.update results ~pure_f:(fun rs -> op_results @ rs);
                 go ops
               | Canceled -> Canceled)
            | [] -> Completed ()
          in
          ignore (go ops : unit Or_canceled.t);
          Atomic.decr running));
      (* Another task just pushing repeatedly, to make sure that we balance pushes and
       pops *)
      Concurrent.spawn s ~f:(fun s _ c ->
        Barrier.await_or_cancel (Concurrent.await c) (Scope.context s) barrier
        |> Or_canceled.completed_exn;
        while
          Atomic.get running > 0 && not (Cancellation.is_canceled (Scope.context s))
        do
          if Stack.For_testing.length t > 0
          then (
            Stack.push t 0;
            Atomic.update expected_results ~pure_f:(fun rs -> 0 :: rs));
          Await.yield (Concurrent.await c)
        done)
      [@nontail]);
    require
      (not (Atomic.get timed_out))
      ~if_false_then_print_s:(lazy [%message "test timed out"]);
    (* One last drain to make sure everything pushed ends up in results *)
    let remaining = Stack.drain t in
    let results = remaining @ Atomic.get results in
    (* Everything we pushed should eventually have been either popped, or still be in the
       stack *)
    let expected_results =
      Atomic.get expected_results
      @ List.concat_map
          ops
          ~f:
            (List.filter_map ~f:(function
              | Operation.Push i -> Some i
              | _ -> None))
    in
    require_equal
      (module struct
        type t = int list [@@deriving equal, sexp_of]
      end)
      (List.sort ~compare:[%compare: int] results)
      (List.sort ~compare:[%compare: int] expected_results)
  ;;
end

module%test _ = Test_with_all_schedulers (Test)
