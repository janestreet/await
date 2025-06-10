open Base
open Portable
open Await
open Await_sync
open Await_test_helpers
open Expect_test_helpers_base

let require_equal ~(at : [%call_pos]) expected actual =
  if expected <> actual
  then
    Stdio.printf
      "Error (%s:%d): expected=%d actual=%d\n%!"
      at.pos_fname
      at.pos_lnum
      expected
      actual
;;

let require ~(at : [%call_pos]) bool =
  if not bool then Stdio.printf "Error (%s:%d)\n%!" at.pos_fname at.pos_lnum
;;

let print t = print_s [%sexp (t : Semaphore.t)]

let spawn = Multicore.spawn
and setup = Await_blocking.with_await

let%expect_test "Invalid arguments to [create]" =
  require_does_raise (fun () -> Semaphore.create (-1));
  [%expect {| (Invalid_argument "Semaphore.create: invalid initial count") |}];
  require_does_raise (fun () -> Semaphore.create (Semaphore.max_value + 1));
  [%expect {| (Invalid_argument "Semaphore.create: invalid initial count") |}]
;;

let%expect_test "Semaphore basics" =
  setup Terminator.never ~f:(fun w ->
    let sem = Semaphore.create Semaphore.max_value in
    require_does_raise (fun () -> Semaphore.release sem);
    [%expect {| (Sys_error "Semaphore.release: overflow") |}];
    require_equal Semaphore.max_value (Semaphore.get_value sem);
    print sem;
    [%expect {| ((value 1152921504606846976)) |}];
    Semaphore.acquire w sem;
    require_equal (Semaphore.max_value - 1) (Semaphore.get_value sem);
    print sem;
    [%expect {| ((value 1152921504606846975)) |}];
    Semaphore.release sem;
    require_equal Semaphore.max_value (Semaphore.get_value sem);
    print sem;
    [%expect {| ((value 1152921504606846976)) |}])
;;

external is_runtime5 : unit -> bool = "%runtime5"

let%expect_test "Semaphore stress" =
  setup Terminator.never ~f:(fun w ->
    let sem = Semaphore.create 0 in
    Structured.with_scope w () ~f:(fun _w s ->
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
        Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun w () ->
          Semaphore.acquire w sem);
        loop ~n_acquire:(n_acquire - 1) ~n_release
      and fork_release ~n_acquire ~n_release =
        Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun _w () ->
          Semaphore.release sem);
        loop ~n_acquire ~n_release:(n_release - 1)
      in
      let n = if is_runtime5 () then 100 else 100 in
      loop ~n_acquire:n ~n_release:n [@nontail]);
    require_equal 0 (Semaphore.get_value sem);
    print sem;
    [%expect {| ((value 0)) |}])
;;

let%expect_test "Semaphore poisoning" =
  setup Terminator.never ~f:(fun w ->
    let sem = Semaphore.create 2 in
    Semaphore.acquire w sem;
    Structured.with_scope w () ~f:(fun w s ->
      Semaphore.acquire w sem;
      for _ = 0 to Random.int 5 do
        Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun w () ->
          match Semaphore.acquire w sem with
          | () -> require false
          | exception Semaphore.Poisoned -> ())
      done;
      Semaphore.poison sem;
      require (Semaphore.is_poisoned sem);
      require_equal 0 (Semaphore.get_value sem);
      print sem;
      [%expect {| ((value 0)) |}];
      Semaphore.release sem;
      require_equal 0 (Semaphore.get_value sem);
      require (Semaphore.is_poisoned sem)))
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
  [%expect {| (Await_sync__Semaphore.Poisoned) |}]
;;

let%expect_test "Semaphore as mutex stress" =
  let sem = Semaphore.create 1 in
  let counter = Stdlib.Obj.magic_portable (ref 0) in
  [ 100; 1_000; 10_000 ]
  |> List.iter ~f:(fun (n_incr_per_domain : int) ->
    [ 1; 2; 8 ]
    |> List.iter ~f:(fun n_domains ->
      setup Terminator.never ~f:(fun w ->
        (*let barrier = Barrier.create n_domains in*)
        Structured.with_scope w () ~f:(fun _w s ->
          for _ = 1 to n_domains do
            Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun w () ->
              (*Barrier.await w barrier;*)
              for _ = 1 to n_incr_per_domain do
                Semaphore.acquire w sem;
                Thread.yield ();
                Int.incr (Stdlib.Obj.magic_uncontended counter);
                Semaphore.release sem
              done)
          done);
        require_equal !counter (n_domains * n_incr_per_domain);
        counter := 0);
      Stdio.printf
        "Done n_domains=%d n_incr_per_domain=%d\n%!"
        n_domains
        n_incr_per_domain));
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
  setup Terminator.never ~f:(fun w ->
    let sem = Semaphore.create 2 in
    Structured.with_scope w () ~f:(fun _w s ->
      let workers =
        Iarray.init n_workers ~f:(fun _ ->
          Terminator.with_ (fun t -> Atomic.make (Option.value_exn (Terminator.source t))))
      in
      Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun _w () ->
        while 0 < Atomic.get terminations_todo || 0 < Atomic.get acquires_todo do
          Thread.yield ();
          let i = Random.int (Iarray.length workers) in
          let t = Atomic.get (Iarray.get workers i) in
          Terminator.Source.terminate t
        done);
      for i = 0 to Iarray.length workers - 1 do
        Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun w () ->
          while 0 < Atomic.get terminations_todo || 0 < Atomic.get acquires_todo do
            try
              Terminator.with_linked (terminator w) (fun t ->
                Option.iter
                  ~f:(fun t -> Atomic.set (Iarray.get workers i) t)
                  (Terminator.source t);
                setup t ~f:(fun w ->
                  while
                    0 < Atomic.get terminations_todo || 0 < Atomic.get acquires_todo
                  do
                    Semaphore.acquire w sem;
                    Thread.yield ();
                    Atomic.decr acquires_todo;
                    Semaphore.release sem
                  done))
            with
            | Terminated -> Atomic.decr terminations_todo
          done)
      done);
    require_equal 2 (Semaphore.get_value sem))
;;
