open Base
open Await
open Await_sync
open Await_test_helpers

let spawn = Multicore.spawn
and setup = Await_blocking.with_await

let%expect_test "Awaitable.await signaled" =
  setup Terminator.never ~f:(fun w ->
    let state = Awaitable.make `Initial in
    let state_is v = phys_equal (Awaitable.get state) v in
    let barrier = Barrier.create 2 in
    Structured.with_scope w () ~f:(fun _w s ->
      Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun w () ->
        Stdio.print_endline "Child running";
        Barrier.await w barrier;
        Awaitable.set state `Child_running;
        Awaitable.signal state;
        if state_is `Child_running
        then
          while state_is `Child_running do
            match Awaitable.await w state ~until_phys_unequal_to:`Child_running with
            | Signaled -> Stdio.print_endline "Child signaled, or parent got to it first"
            | Terminated ->
              Stdio.print_endline "Child terminated";
              raise Terminated
          done
        else Stdio.print_endline "Child signaled, or parent got to it first");
      Barrier.await w barrier;
      if state_is `Initial
      then
        while state_is `Initial do
          match Awaitable.await w state ~until_phys_unequal_to:`Initial with
          | Signaled -> Stdio.print_endline "Parent signaled, or child got to it first"
          | Terminated ->
            Stdio.print_endline "Parent terminated";
            raise Terminated
        done
      else Stdio.print_endline "Parent signaled, or child got to it first";
      Awaitable.set state `Stop;
      Awaitable.signal state)
    [@nontail]);
  [%expect
    {|
    Child running
    Parent signaled, or child got to it first
    Child signaled, or parent got to it first
    |}]
;;

let%expect_test "Awaitable.await terminated" =
  setup Terminator.never ~f:(fun w ->
    let state = Awaitable.make `Initial in
    let state_is v = phys_equal (Awaitable.get state) v in
    Structured.with_scope w () ~f:(fun w s ->
      Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun w () ->
        Stdio.print_endline "Child running";
        Awaitable.set state `Child_running;
        Awaitable.signal state;
        while state_is `Child_running do
          match Awaitable.await w state ~until_phys_unequal_to:`Child_running with
          | Signaled -> Stdio.print_endline "Child signaled"
          | Terminated ->
            Stdio.print_endline "Child terminated";
            raise Terminated
        done);
      if state_is `Initial
      then
        while state_is `Initial do
          match Awaitable.await w state ~until_phys_unequal_to:`Initial with
          | Signaled -> Stdio.print_endline "Parent signaled, or child got to it first"
          | Terminated -> Stdio.print_endline "Parent terminated"
        done
      else Stdio.print_endline "Parent signaled, or child got to it first";
      Option.(iter [@mode local])
        ~f:Terminator.Source.terminate
        (Terminator.source (terminator w));
      if not (Terminator.is_terminated (terminator w))
      then Stdio.print_endline "Scope not terminated")
    [@nontail]);
  [%expect
    {|
    Child running
    Parent signaled, or child got to it first
    Child terminated
    |}]
;;

let%expect_test "Awaitable.await canceled" =
  setup Terminator.never ~f:(fun w ->
    let state = Awaitable.make `Initial in
    let state_is v = phys_equal (Awaitable.get state) v in
    Cancellation.with_ (fun c ->
      let c = Cancellation.Expert.globalize c in
      Structured.with_scope w c ~f:(fun w s ->
        Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun w c ->
          Stdio.print_endline "Child running";
          Awaitable.set state `Child_running;
          Awaitable.signal state;
          while state_is `Child_running do
            match
              Awaitable.await_or_cancel w c state ~until_phys_unequal_to:`Child_running
            with
            | Signaled -> Stdio.print_endline "Child signaled"
            | Terminated ->
              Stdio.print_endline "Child terminated";
              raise Terminated
            | Canceled ->
              Stdio.print_endline "Child canceled";
              raise Terminated
          done);
        if state_is `Initial
        then
          while state_is `Initial do
            match Awaitable.await w state ~until_phys_unequal_to:`Initial with
            | Signaled -> Stdio.print_endline "Parent signaled, or child got to it first"
            | Terminated -> Stdio.print_endline "Parent terminated"
          done
        else Stdio.print_endline "Parent signaled, or child got to it first";
        Option.(iter [@mode local]) ~f:Cancellation.Source.cancel (Cancellation.source c);
        if not (Cancellation.is_canceled c) then Stdio.print_endline "Token not canceled"))
    [@nontail]);
  [%expect
    {|
    Child running
    Parent signaled, or child got to it first
    Child canceled
    |}]
;;
