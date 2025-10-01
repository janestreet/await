open Core
open Import
open Portable
open Await
open Expect_test_helpers_base

let print t = print_s [%sexp (t : Semaphore.t)]

let%expect_test "Invalid arguments to [create]" =
  require_does_raise (fun () -> Semaphore.create (-1));
  [%expect {| (Invalid_argument "Semaphore.create: invalid initial count") |}];
  require_does_raise (fun () -> Semaphore.create (Semaphore.max_value + 1));
  [%expect {| (Invalid_argument "Semaphore.create: invalid initial count") |}]
;;

let%expect_test "Semaphore basics" =
  let%with.tilde.stack w = Await_blocking.with_await Terminator.never in
  let sem = Semaphore.create Semaphore.max_value in
  require_does_raise (fun () -> Semaphore.release sem);
  [%expect {| (Sys_error "Semaphore.release: overflow") |}];
  require_equal (module Int) Semaphore.max_value (Semaphore.get_value sem);
  print sem;
  [%expect {| ((value 1152921504606846976)) |}];
  Semaphore.acquire w sem;
  require_equal (module Int) (Semaphore.max_value - 1) (Semaphore.get_value sem);
  print sem;
  [%expect {| ((value 1152921504606846975)) |}];
  Semaphore.release sem;
  require_equal (module Int) Semaphore.max_value (Semaphore.get_value sem);
  print sem;
  [%expect {| ((value 1152921504606846976)) |}]
;;

let%expect_test "Semaphore try_acquire" =
  let sem = Semaphore.create 2 in
  print_s [%sexp (Semaphore.try_acquire sem : Semaphore.Acquired_or_would_block.t)];
  [%expect {| Acquired |}];
  print_s [%sexp (Semaphore.try_acquire sem : Semaphore.Acquired_or_would_block.t)];
  [%expect {| Acquired |}];
  print_s [%sexp (Semaphore.try_acquire sem : Semaphore.Acquired_or_would_block.t)];
  [%expect {| Would_block |}];
  Semaphore.release sem;
  print_s [%sexp (Semaphore.try_acquire sem : Semaphore.Acquired_or_would_block.t)];
  [%expect {| Acquired |}];
  Semaphore.poison sem;
  require_does_raise (fun () -> Semaphore.try_acquire sem);
  [%expect {| (Poisoned) |}]
;;

module Test (S : S) = struct
  let%expect_test "Semaphore stress" =
    S.with_concurrent ~f:(fun c ->
      let sem = Semaphore.create 0 in
      Concurrent.with_scope c () ~f:(fun s ->
        let rec loop ~n_acquire ~n_release =
          if 0 < n_acquire && 0 < n_release
          then
            if Random.bool ()
            then fork_acquire ~n_acquire ~n_release
            else fork_release ~n_acquire ~n_release
          else if 0 < n_acquire
          then fork_acquire ~n_acquire ~n_release
          else if 0 < n_release
          then fork_release ~n_acquire ~n_release
        and fork_acquire ~n_acquire ~n_release =
          Concurrent.spawn s ~f:(fun _ _ c ->
            Semaphore.acquire (Concurrent.await c) sem [@nontail]);
          loop ~n_acquire:(n_acquire - 1) ~n_release
        and fork_release ~n_acquire ~n_release =
          Concurrent.spawn s ~f:(fun _ _ _ -> Semaphore.release sem);
          loop ~n_acquire ~n_release:(n_release - 1)
        in
        let n = if S.scalable then 100_000 else 100 in
        loop ~n_acquire:n ~n_release:n [@nontail]);
      require_equal (module Int) 0 (Semaphore.get_value sem);
      print sem);
    [%expect {| ((value 0)) |}]
  ;;

  let%expect_test "Semaphore poisoning" =
    S.with_concurrent ~f:(fun c ->
      let sem = Semaphore.create 2 in
      Semaphore.acquire (Concurrent.await c) sem;
      Concurrent.with_scope c () ~f:(fun s ->
        Semaphore.acquire (Concurrent.await c) sem;
        for _ = 0 to Random.int 5 do
          Concurrent.spawn s ~f:(fun _ _ c ->
            match Semaphore.acquire (Concurrent.await c) sem with
            | () -> require false
            | exception Poisoned -> ())
        done;
        Semaphore.poison sem;
        require (Semaphore.is_poisoned sem);
        require_equal (module Int) 0 (Semaphore.get_value sem);
        print sem;
        Semaphore.release sem;
        require_equal (module Int) 0 (Semaphore.get_value sem);
        require (Semaphore.is_poisoned sem))
      [@nontail]);
    [%expect {| ((value 0)) |}]
  ;;

  let%expect_test "Semaphore as mutex stress" =
    let sem = Semaphore.create 1 in
    let counter = Stdlib.Obj.magic_portable (ref 0) in
    [ 100; 1_000; 10_000 ]
    |> List.iter ~f:(fun (n_incr_per_domain : int) ->
      [ 1; 2; 8 ]
      |> List.iter ~f:(fun n_domains ->
        S.with_concurrent ~f:(fun c ->
          (*let barrier = Barrier.create n_domains in*)
          Concurrent.with_scope c () ~f:(fun s ->
            for _ = 1 to n_domains do
              Concurrent.spawn s ~f:(fun _ _ c ->
                (*Barrier.await (Concurrent.await c) barrier;*)
                for _ = 1 to n_incr_per_domain do
                  Semaphore.acquire (Concurrent.await c) sem;
                  Core_thread.yield ();
                  Int.incr (Stdlib.Obj.magic_uncontended counter);
                  Semaphore.release sem
                done)
            done));
        require_equal (module Int) !counter (n_domains * n_incr_per_domain);
        counter := 0;
        printf "Done n_domains=%d n_incr_per_domain=%d\n%!" n_domains n_incr_per_domain));
    [%expect
      {|
      Done n_domains=1 n_incr_per_domain=100
      Done n_domains=2 n_incr_per_domain=100
      Done n_domains=8 n_incr_per_domain=100
      Done n_domains=1 n_incr_per_domain=1000
      Done n_domains=2 n_incr_per_domain=1000
      Done n_domains=8 n_incr_per_domain=1000
      Done n_domains=1 n_incr_per_domain=10000
      Done n_domains=2 n_incr_per_domain=10000
      Done n_domains=8 n_incr_per_domain=10000
      |}]
  ;;

  let%expect_test "Semaphore termination" =
    let n_workers = 4 in
    let n_terminations = 1_000 in
    let n_acquires = 1_000 in
    let terminations_todo = Atomic.make n_terminations in
    let acquires_todo = Atomic.make n_acquires in
    let%with.tilde c = S.with_concurrent in
    let sem = Semaphore.create 2 in
    Concurrent.with_scope c () ~f:(fun s ->
      let workers =
        Iarray.init n_workers ~f:(fun _ ->
          (Terminator.with_ (fun t ->
             { aliased = Atomic.make (Or_null.value_exn (Terminator.source t)) }))
            .aliased)
      in
      Concurrent.spawn s ~f:(fun _ _ _c ->
        while 0 < Atomic.get terminations_todo || 0 < Atomic.get acquires_todo do
          Core_thread.yield ();
          let i = Random.int (Iarray.length workers) in
          let t = Atomic.get (Iarray.get workers i) in
          Terminator.Source.terminate t
        done);
      for i = 0 to Iarray.length workers - 1 do
        Concurrent.spawn s ~f:(fun _ _ c ->
          while 0 < Atomic.get terminations_todo || 0 < Atomic.get acquires_todo do
            try
              Terminator.with_linked
                (Await.terminator (Concurrent.await c))
                (fun t ->
                  Or_null.iter
                    ~f:(fun t -> Atomic.set (Iarray.get workers i) t)
                    (Terminator.source t);
                  Await_blocking.with_await t ~f:(fun w ->
                    while
                      0 < Atomic.get terminations_todo || 0 < Atomic.get acquires_todo
                    do
                      Semaphore.acquire w sem;
                      Core_thread.yield ();
                      Atomic.decr acquires_todo;
                      Semaphore.release sem
                    done))
            with
            | Await.Terminated -> Atomic.decr terminations_todo
          done)
      done);
    require_equal (module Int) 2 (Semaphore.get_value sem)
  ;;
end

module%test _ = Test_with_all_schedulers (Test)
