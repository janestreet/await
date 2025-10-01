open Core
open Import
open Await
open Expect_test_helpers_base

module Test (S : S) = struct
  let%expect_test "Barrier basics" =
    S.with_concurrent ~f:(fun c ->
      for n = 1 to 4 do
        printf "%d %!" n;
        let barrier = Barrier.create n in
        Concurrent.with_scope c () ~f:(fun s ->
          let n_outside = Atomic.make n in
          for _ = 1 to n do
            Concurrent.spawn s ~f:(fun _ _ conc ->
              for _ = 1 to 5 do
                Atomic.decr n_outside;
                Barrier.await (Concurrent.await conc) barrier;
                let n = Atomic.get n_outside in
                if n <> 0 then printf "Error: %d%!" n;
                Barrier.await (Concurrent.await conc) barrier;
                Atomic.incr n_outside
              done)
          done)
        [@nontail]
      done);
    [%expect {| 1 2 3 4 |}]
  ;;

  let%expect_test "Barrier poisoning" =
    S.with_concurrent ~f:(fun c ->
      for n = 2 to 5 do
        require_does_raise (fun () ->
          let barrier = Barrier.create n in
          Concurrent.with_scope c () ~f:(fun s ->
            for _ = 2 to n do
              Concurrent.spawn s ~f:(fun _ _ conc ->
                Barrier.await (Concurrent.await conc) barrier;
                print_cr [%message "Barrier.await returned"])
            done;
            Barrier.poison barrier))
      done);
    [%expect
      {|
      (Poisoned)
      (Poisoned)
      (Poisoned)
      (Poisoned)
      |}]
  ;;

  let%expect_test "More barrier poisoning" =
    S.with_concurrent ~f:(fun c ->
      for n = 3 to 6 do
        require_does_raise (fun () ->
          let barrier = Barrier.create n in
          Concurrent.with_scope c () ~f:(fun s ->
            Concurrent.spawn s ~f:(fun _ _ c ->
              Concurrent.with_scope c () ~f:(fun s ->
                Concurrent.spawn s ~f:(fun _ _ c ->
                  Barrier.await (Concurrent.await c) barrier;
                  print_cr [%message "Barrier.await returned"]);
                failwith "expected")
              [@nontail]);
            for _ = 3 to n do
              Concurrent.spawn s ~f:(fun _ _ c ->
                match Barrier.await (Concurrent.await c) barrier with
                | exception Poisoned -> ()
                | () -> print_cr [%message "Barrier.await returned"])
            done)
          [@nontail])
      done);
    [%expect
      {|
      (Failure expected)
      (Failure expected)
      (Failure expected)
      (Failure expected)
      |}]
  ;;
end

module%test _ = Test_with_all_schedulers (Test)
