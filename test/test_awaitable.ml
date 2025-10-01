open Base
open Import
open Await
open Expect_test_helpers_base

module Test (S : S) = struct
  let%expect_test "Awaitable.await signaled" =
    S.with_concurrent ~f:(fun conc ->
      let state = Awaitable.make `Initial in
      let state_is v = phys_equal (Awaitable.get state) v in
      let barrier = Barrier.create 2 in
      Concurrent.with_scope conc () ~f:(fun s ->
        Concurrent.spawn s ~f:(fun _ _ conc ->
          print_endline "Child running";
          Barrier.await (Concurrent.await conc) barrier;
          Awaitable.set state `Child_running;
          Awaitable.signal state;
          if state_is `Child_running
          then
            while state_is `Child_running do
              match
                Awaitable.await
                  (Concurrent.await conc)
                  state
                  ~until_phys_unequal_to:`Child_running
              with
              | Signaled -> print_endline "Child signaled, or parent got to it first"
              | Terminated ->
                print_endline "Child terminated";
                raise Await.Terminated
            done
          else print_endline "Child signaled, or parent got to it first");
        Barrier.await (Concurrent.await conc) barrier;
        if state_is `Initial
        then
          while state_is `Initial do
            match
              Awaitable.await
                (Concurrent.await conc)
                state
                ~until_phys_unequal_to:`Initial
            with
            | Signaled -> print_endline "Parent signaled, or child got to it first"
            | Terminated ->
              print_endline "Parent terminated";
              raise Await.Terminated
          done
        else print_endline "Parent signaled, or child got to it first";
        Awaitable.set state `Stop;
        Awaitable.signal state)
      [@nontail]);
    [%expect
      {|
      Child running
      Parent signaled, or child got to it first
      Child signaled, or parent got to it first
      |}];
    [%expect {| |}]
  ;;

  let%expect_test "Awaitable.await terminated" =
    S.with_concurrent ~f:(fun conc ->
      let state = Awaitable.make `Initial in
      let state_is v = phys_equal (Awaitable.get state) v in
      require_does_raise (fun () ->
        Concurrent.with_scope conc () ~f:(fun s ->
          Concurrent.spawn s ~f:(fun _ _ conc ->
            print_endline "Child running";
            Awaitable.set state `Child_running;
            Awaitable.signal state;
            while state_is `Child_running do
              match
                Awaitable.await
                  (Concurrent.await conc)
                  state
                  ~until_phys_unequal_to:`Child_running
              with
              | Signaled -> print_endline "Child signaled"
              | Terminated ->
                print_endline "Child terminated";
                raise Await.Terminated
            done);
          if state_is `Initial
          then
            while state_is `Initial do
              match
                Awaitable.await
                  (Concurrent.await conc)
                  state
                  ~until_phys_unequal_to:`Initial
              with
              | Signaled -> print_endline "Parent signaled, or child got to it first"
              | Terminated -> print_endline "Parent terminated"
            done
          else print_endline "Parent signaled, or child got to it first";
          let t = Concurrent.Spawn.terminator s in
          Or_null.(iter [@mode local])
            ~f:Terminator.Source.terminate
            (Terminator.source t);
          if not (Terminator.is_terminated t) then print_endline "Scope not terminated")
        [@nontail])
      [@nontail]);
    [%expect
      {|
      Child running
      Parent signaled, or child got to it first
      Child terminated
      (Terminated)
      |}]
  ;;

  let%expect_test "Awaitable.await canceled" =
    S.with_concurrent ~f:(fun conc ->
      let state = Awaitable.make `Initial in
      let state_is v = phys_equal (Awaitable.get state) v in
      Cancellation.with_ (fun c ->
        let c = Cancellation.Expert.globalize c in
        require_does_raise (fun () ->
          Concurrent.with_scope conc c ~f:(fun s ->
            Concurrent.spawn s ~f:(fun c _ conc ->
              print_endline "Child running";
              Awaitable.set state `Child_running;
              Awaitable.signal state;
              while state_is `Child_running do
                match
                  Awaitable.await_or_cancel
                    (Concurrent.await conc)
                    (Scope.context c)
                    state
                    ~until_phys_unequal_to:`Child_running
                with
                | Signaled -> print_endline "Child signaled"
                | Terminated ->
                  print_endline "Child terminated";
                  raise Await.Terminated
                | Canceled ->
                  print_endline "Child canceled";
                  raise Await.Terminated
              done);
            if state_is `Initial
            then
              while state_is `Initial do
                match
                  Awaitable.await
                    (Concurrent.await conc)
                    state
                    ~until_phys_unequal_to:`Initial
                with
                | Signaled -> print_endline "Parent signaled, or child got to it first"
                | Terminated -> print_endline "Parent terminated"
              done
            else print_endline "Parent signaled, or child got to it first";
            Or_null.(iter [@mode local])
              ~f:Cancellation.Source.cancel
              (Cancellation.source c);
            if not (Cancellation.is_canceled c) then print_endline "Token not canceled")
          [@nontail])
        [@nontail])
      [@nontail]);
    [%expect
      {|
      Child running
      Parent signaled, or child got to it first
      Child canceled
      (Terminated)
      |}]
  ;;
end

module%test _ = Test_with_all_schedulers (Test)
