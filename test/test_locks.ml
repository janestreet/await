open Core
open Basement
open Await
open Caml_threads
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

let spawn = Multicore.spawn
and setup = Await_blocking.with_await

module type Lock = sig @@ portable
  type 'k t : value mod contended portable

  module Guard : sig
    type 'k t : value mod contended portable

    val poison : 'k t @ unique -> 'k Capsule.Key.t @ unique
    val release : 'k t @ unique -> unit
  end

  val create : 'k Capsule.Key.t @ unique -> 'k t

  val with_access
    :  Await.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Access.t -> 'a @ contended portable unique) @ local once portable
    -> 'a @ contended once portable unique

  val with_access_poisoning
    :  Await.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Access.t -> 'a @ contended portable unique) @ local once portable
    -> 'a @ contended once portable unique

  val acquire : Await.t @ local -> 'k t -> 'k Guard.t @ unique

  exception Poisoned

  val is_poisoned : 'k t @ local -> bool

  module For_testing : sig
    val length : 'k t @ local -> int
    val is_locked : 'k t @ local -> bool
  end
end

module Test_lock (Lock : Lock) = struct
  let%expect_test "Lock stress" =
    let (Capsule.Key.P key) = Capsule.create () in
    let lock = Lock.create key in
    let counter = Capsule.Data.create (fun () -> ref 0) in
    [ 100; 1_000; 10_000 ]
    |> List.iter ~f:(fun (n_incr_per_domain : int) ->
      [ 1; 2; 8 ]
      |> List.iter ~f:(fun n_domains ->
        setup Terminator.never ~f:(fun w ->
          let barrier = Barrier.create n_domains in
          Structured.with_scope w () ~f:(fun _w s ->
            for _ = 1 to n_domains do
              Structured.Scope.(add [@mode portable local])
                ~spawn
                ~setup
                s
                ~f:(fun w () ->
                  Barrier.await w barrier;
                  for _ = 1 to n_incr_per_domain do
                    Lock.with_access w lock ~f:(fun access ->
                      let counter = Capsule.Data.unwrap counter ~access in
                      Int.incr counter)
                  done)
            done);
          require_equal 0 (Lock.For_testing.length lock);
          Lock.with_access w lock ~f:(fun access ->
            let counter = Capsule.Data.unwrap counter ~access in
            require_equal !counter (n_domains * n_incr_per_domain);
            counter := 0));
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

  let%expect_test "Lock termination" =
    let n_workers = 4 in
    let n_terminations = 1_000 in
    let n_acquires = 1_000 in
    let terminations_todo = Atomic.make n_terminations in
    let acquires_todo = Atomic.make n_acquires in
    setup Terminator.never ~f:(fun w ->
      let (Capsule.Key.P key) = Capsule.create () in
      let lock = Lock.create key in
      Structured.with_scope w () ~f:(fun _w s ->
        let workers =
          Iarray.init n_workers ~f:(fun _ ->
            Terminator.with_ (fun t ->
              Atomic.make (Option.value_exn (Terminator.source t))))
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
                      let guard = Lock.acquire w lock in
                      Thread.yield ();
                      Atomic.decr acquires_todo;
                      Lock.Guard.release guard
                    done))
              with
              | Terminated -> Atomic.decr terminations_todo
            done)
        done);
      Stdio.printf "Done\n%!");
    [%expect {| Done |}]
  ;;

  let%expect_test "lock can't be acquired if it's explicitly poisoned" =
    setup Terminator.never ~f:(fun w ->
      let (Capsule.Key.P key) = Capsule.create () in
      let lock = Lock.create key in
      Structured.with_scope w () ~f:(fun w s ->
        let guard : _ Lock.Guard.t = Lock.acquire w lock in
        for _ = 0 to Random.int 3 do
          Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun w () ->
            match Lock.acquire w lock with
            | _ -> require false
            | exception Lock.Poisoned -> ())
        done;
        let _key : _ Capsule.Key.t = Lock.Guard.poison guard in
        require (Lock.For_testing.is_locked lock);
        require (Lock.is_poisoned lock));
      Stdio.printf "Done\n%!");
    [%expect {| Done |}]
  ;;

  let%expect_test "lock poisons if with_access_poisoning raises" =
    setup Terminator.never ~f:(fun w ->
      let (Capsule.Key.P key) = Capsule.create () in
      let lock = Lock.create key in
      require_does_raise (fun () ->
        Lock.with_access_poisoning w lock ~f:(fun (_access : _ Capsule.Access.t) ->
          { contended = { many = { aliased = failwith "Poison this lock!" } } }));
      [%expect {| (Failure "Poison this lock!") |}];
      print_s [%sexp (Lock.is_poisoned lock : bool)];
      [%expect {| true |}];
      require_does_raise (fun () ->
        Lock.with_access w lock ~f:(fun (_access : _ Capsule.Access.t) -> ()));
      [%expect {| (Await_sync__Mutex.Poisoned) |}])
  ;;

  let%expect_test "lock does not poison if with_access raises" =
    setup Terminator.never ~f:(fun w ->
      let (Capsule.Key.P key) = Capsule.create () in
      let lock = Lock.create key in
      require_does_raise (fun () ->
        Lock.with_access w lock ~f:(fun (_access : _ Capsule.Access.t) ->
          { contended = { many = { aliased = failwith "Don't poison this lock!" } } }));
      [%expect {| (Failure "Don't poison this lock!") |}];
      print_s [%sexp (Lock.is_poisoned lock : bool)];
      [%expect {| false |}];
      require_does_not_raise (fun () ->
        Lock.with_access w lock ~f:(fun (_access : _ Capsule.Access.t) -> ()));
      [%expect {| |}])
  ;;

  let%expect_test "guard finalizer poisons the lock" =
    setup Terminator.never ~f:(fun w ->
      let (P key) = Capsule.create () in
      let lock = Lock.create key in
      let guard = Lock.acquire w lock in
      (* Leak the guard *)
      ignore (guard : _ Lock.Guard.t);
      (* Ensure it's GC'd *)
      Gc.full_major ();
      Gc.full_major ();
      (* Now the lock should not be poisoned, because the finaliser doesn't poison by default *)
      print_s [%sexp (Lock.is_poisoned lock : bool)];
      [%expect {| true |}])
  ;;

  let%expect_test "guard doesn't poison the lock if release is called" =
    setup Terminator.never ~f:(fun w ->
      let (P key) = Capsule.create () in
      let lock = Lock.create key in
      let guard = Lock.acquire w lock in
      Lock.Guard.release guard;
      (* Ensure it's GC'd *)
      Gc.full_major ();
      Gc.full_major ();
      (* The lock should not be poisoned, even though the finaliser ran *)
      print_s [%sexp (Lock.is_poisoned lock : bool)];
      [%expect {| false |}])
  ;;

  let%expect_test "multiple guards in sequence" =
    setup Terminator.never ~f:(fun w ->
      let (P key) = Capsule.create () in
      let lock = Lock.create key in
      let guard = Lock.acquire w lock in
      Lock.Guard.release guard;
      let guard = Lock.acquire w lock in
      (* Ensure the first guard is GC'd *)
      Gc.full_major ();
      Gc.full_major ();
      Lock.Guard.release guard;
      (* The lock should not be poisoned, even though the finalizer for the first guard
         (probably) ran while the second guard had the mutex unlocked. *)
      print_s [%sexp (Lock.is_poisoned lock : bool)];
      [%expect {| false |}])
  ;;
end

module%test Test_mutex = Test_lock (Mutex)
