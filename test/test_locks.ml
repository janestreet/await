open Core
open Caml_threads
open Import
open Await
open Expect_test_helpers_base
module Capsule = Capsule.Expert

let full_major = Obj.magic_portable Gc.full_major

module type Lock = sig @@ portable
  type 'k t : value mod contended portable

  module Guard : sig
    type 'k t : value mod contended portable

    val poison : 'k t @ unique -> 'k Capsule.Key.t @ unique
    val release : 'k t @ unique -> unit
  end

  val create : 'k Capsule.Key.t @ unique -> 'k t

  val with_key
    : ('a : value_or_null) 'k.
    Await.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ once unique)
       @ local once
    -> 'a @ once unique

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
  val is_poisoned : 'k t @ local -> bool
  val poison : 'k t @ local -> 'k Capsule.Key.t @ unique -> 'k Capsule.Key.t @ unique

  module For_testing : sig
    val length : 'k t @ local -> int
    val is_exclusive : 'k t @ local -> bool
  end
end

module Test_locks (S : S) = struct
  module Test_lock (Lock : Lock) = struct
    let%expect_test "Lock stress" =
      let (Capsule.Key.P key) = Capsule.create () in
      let lock = Lock.create key in
      let counter = Capsule.Data.create (fun () -> ref 0) in
      [ 100; 1_000; 10_000 ]
      |> List.iter ~f:(fun (n_incr_per_domain : int) ->
        [ 1; 2; 8 ]
        |> List.iter ~f:(fun n_domains ->
          S.with_concurrent ~f:(fun c ->
            let barrier = Barrier.create n_domains in
            Concurrent.with_scope c () ~f:(fun s ->
              for _ = 1 to n_domains do
                Concurrent.spawn s ~f:(fun _ _ c ->
                  Barrier.await (Concurrent.await c) barrier;
                  for _ = 1 to n_incr_per_domain do
                    Lock.with_access (Concurrent.await c) lock ~f:(fun access ->
                      let counter = Capsule.Data.unwrap counter ~access in
                      Int.incr counter)
                  done)
              done);
            require_equal (module Int) 0 (Lock.For_testing.length lock);
            Lock.with_access (Concurrent.await c) lock ~f:(fun access ->
              let counter = Capsule.Data.unwrap counter ~access in
              require_equal (module Int) !counter (n_domains * n_incr_per_domain);
              counter := 0)
            [@nontail];
            printf
              "Done n_domains=%d n_incr_per_domain=%d\n%!"
              n_domains
              n_incr_per_domain)));
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
      S.with_concurrent ~f:(fun c ->
        let (Capsule.Key.P key) = Capsule.create () in
        let lock = Lock.create key in
        Concurrent.with_scope c () ~f:(fun s ->
          let workers =
            Iarray.init n_workers ~f:(fun _ ->
              (Terminator.with_ (fun t ->
                 { aliased = Atomic.make (Or_null.value_exn (Terminator.source t)) }))
                .aliased)
          in
          Concurrent.spawn s ~f:(fun _ _ _c ->
            while 0 < Atomic.get terminations_todo || 0 < Atomic.get acquires_todo do
              Thread.yield ();
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
                          let guard = Lock.acquire w lock in
                          Thread.yield ();
                          Atomic.decr acquires_todo;
                          Lock.Guard.release guard
                        done))
                with
                | Await.Terminated -> Atomic.decr terminations_todo
              done)
          done);
        printf "Done\n%!");
      [%expect {| Done |}]
    ;;

    let%expect_test "lock can't be acquired if it's explicitly poisoned" =
      S.with_concurrent ~f:(fun c ->
        let (Capsule.Key.P key) = Capsule.create () in
        let lock = Lock.create key in
        Concurrent.with_scope c () ~f:(fun s ->
          let guard : _ Lock.Guard.t = Lock.acquire (Concurrent.await c) lock in
          for _ = 0 to Random.int 3 do
            Concurrent.spawn s ~f:(fun _ _ c ->
              match Lock.acquire (Concurrent.await c) lock with
              | _ -> require false
              | exception Poisoned -> ())
          done;
          let _key : _ Capsule.Key.t = Lock.Guard.poison guard in
          require (Lock.For_testing.is_exclusive lock);
          require (Lock.is_poisoned lock));
        printf "Done\n%!");
      [%expect {| Done |}]
    ;;

    let%expect_test "lock poisons if with_access_poisoning raises" =
      S.with_await ~f:(fun w ->
        let (Capsule.Key.P key) = Capsule.create () in
        let lock = Lock.create key in
        require_does_raise (fun () ->
          Lock.with_access_poisoning w lock ~f:(fun (_access : _ Capsule.Access.t) ->
            { contended = { many = { aliased = failwith "Poison this lock!" } } }));
        print_s [%sexp { is_poisoned = (Lock.is_poisoned lock : bool) }];
        require_does_raise (fun () ->
          Lock.with_access w lock ~f:(fun (_access : _ Capsule.Access.t) -> ()))
        [@nontail]);
      [%expect
        {|
        (Failure "Poison this lock!")
        ((is_poisoned true))
        (Poisoned)
        |}]
    ;;

    let%expect_test "lock does not poison if with_access raises" =
      S.with_await ~f:(fun w ->
        let (Capsule.Key.P key) = Capsule.create () in
        let lock = Lock.create key in
        require_does_raise (fun () ->
          Lock.with_access w lock ~f:(fun (_access : _ Capsule.Access.t) ->
            { contended = { many = { aliased = failwith "Don't poison this lock!" } } }));
        print_s [%sexp { is_poisoned = (Lock.is_poisoned lock : bool) }];
        require_does_not_raise (fun () ->
          Lock.with_access w lock ~f:(fun (_access : _ Capsule.Access.t) -> ()))
        [@nontail]);
      [%expect
        {|
        (Failure "Don't poison this lock!")
        ((is_poisoned false))
        |}]
    ;;

    let%expect_test "guard finalizer poisons the lock" =
      S.with_await ~f:(fun w ->
        let (P key) = Capsule.create () in
        let lock = Lock.create key in
        let guard = Lock.acquire w lock in
        (* Leak the guard *)
        ignore (guard : _ Lock.Guard.t);
        (* Ensure it's GC'd *)
        full_major ();
        full_major ();
        (* Now the lock should not be poisoned, because the finaliser doesn't poison by default *)
        print_s [%sexp (Lock.is_poisoned lock : bool)]);
      [%expect {| true |}]
    ;;

    let%expect_test "guard doesn't poison the lock if release is called" =
      S.with_await ~f:(fun w ->
        let (P key) = Capsule.create () in
        let lock = Lock.create key in
        let guard = Lock.acquire w lock in
        Lock.Guard.release guard;
        (* Ensure it's GC'd *)
        full_major ();
        full_major ();
        (* The lock should not be poisoned, even though the finaliser ran *)
        print_s [%sexp (Lock.is_poisoned lock : bool)]);
      [%expect {| false |}]
    ;;

    let%expect_test "multiple guards in sequence" =
      S.with_await ~f:(fun w ->
        let (P key) = Capsule.create () in
        let lock = Lock.create key in
        let guard = Lock.acquire w lock in
        Lock.Guard.release guard;
        let guard = Lock.acquire w lock in
        (* Ensure the first guard is GC'd *)
        full_major ();
        full_major ();
        Lock.Guard.release guard;
        (* The lock should not be poisoned, even though the finalizer for the first guard
         (probably) ran while the second guard had the mutex unlocked. *)
        print_s [%sexp (Lock.is_poisoned lock : bool)]);
      [%expect {| false |}]
    ;;
  end

  module%test Test_mutex = Test_lock (Mutex)

  module%test Test_rwlock = struct
    include Test_lock (Rwlock)

    let%expect_test "raising from with_key_shared doesn't freeze the lock" =
      S.with_await ~f:(fun w ->
        let (P key) = Capsule.create () in
        let lock = Rwlock.create key in
        require_does_raise (fun () ->
          Rwlock.with_key_shared w lock ~f:(fun _key ->
            { many = { aliased = failwith "Don't freeze this lock!" } }));
        print_s
          [%sexp
            { frozen = (Rwlock.is_frozen lock : bool)
            ; poisoned = (Rwlock.is_poisoned lock : bool)
            }];
        (* Acquiring for write should not raise *)
        require_does_not_raise (fun () -> Rwlock.with_access w lock ~f:(fun _key -> ()));
        (* Acquiring for read should also not raise *)
        require_does_not_raise (fun () ->
          Rwlock.with_access_shared w lock ~f:(fun _key -> ()))
        [@nontail]);
      [%expect
        {|
        (Failure "Don't freeze this lock!")
        ((frozen   false)
         (poisoned false))
        |}]
    ;;

    let%expect_test "raising from with_key_shared_freezing freezes the lock" =
      S.with_await ~f:(fun w ->
        let (P key) = Capsule.create () in
        let lock = Rwlock.create key in
        require_does_raise (fun () ->
          Rwlock.with_key_shared_freezing w lock ~f:(fun _key ->
            { many = { aliased = failwith "Freeze this lock!" } }));
        print_s
          [%sexp
            { frozen = (Rwlock.is_frozen lock : bool)
            ; poisoned = (Rwlock.is_poisoned lock : bool)
            }];
        (* Now, acquiring for write should raise *)
        require_does_raise (fun () -> Rwlock.with_access w lock ~f:(fun _key -> ()));
        (* Acquiring for read should not raise, though. *)
        require_does_not_raise (fun () ->
          Rwlock.with_access_shared w lock ~f:(fun _key -> ()))
        [@nontail]);
      [%expect
        {|
        (Failure "Freeze this lock!")
        ((frozen   true)
         (poisoned false))
        (Frozen)
        |}]
    ;;

    let%expect_test "shared guard finalizer freezes the lock" =
      S.with_await ~f:(fun w ->
        let (P key) = Capsule.create () in
        let lock = Rwlock.create key in
        let guard = Rwlock.acquire_shared w lock in
        (* Leak the guard *)
        ignore (guard : _ Rwlock.Shared_guard.t);
        (* Ensure it's GC'd *)
        full_major ();
        full_major ();
        (* Now the lock should be frozen *)
        print_s [%sexp { is_frozen = (Rwlock.is_frozen lock : bool) }];
        (* Trying to acquire it for writing should fail *)
        require_does_raise (fun () ->
          Rwlock.with_access w lock ~f:(fun (_access : _ Capsule.Access.t) -> ()));
        (* Acquiring it for reading should still work *)
        require_does_not_raise (fun () ->
          Rwlock.with_access_shared w lock ~f:(fun (_access : _ Capsule.Access.t) -> ()))
        [@nontail]);
      [%expect
        {|
        ((is_frozen true))
        (Frozen)
        |}]
    ;;

    let%expect_test "shared guard doesn't freeze the lock if release is called" =
      S.with_await ~f:(fun w ->
        let (P key) = Capsule.create () in
        let lock = Rwlock.create key in
        let guard = Rwlock.acquire_shared w lock in
        Rwlock.Shared_guard.release guard;
        (* Ensure it's GC'd *)
        full_major ();
        full_major ();
        (* The lock should not be poisoned, even though the finaliser ran *)
        print_s [%sexp (Rwlock.is_frozen lock : bool)]);
      [%expect {| false |}]
    ;;

    let%expect_test "guard can be downgraded to a shared guard" =
      S.with_concurrent ~f:(fun c ->
        let (P key) = Capsule.create () in
        let lock = Rwlock.create key in
        List.iter [ 0; 1; 2 ] ~f:(fun n_exclusive ->
          List.iter [ 0; 1; 2 ] ~f:(fun n_shared ->
            Concurrent.with_scope c () ~f:(fun s ->
              let guard = Rwlock.acquire (Concurrent.await c) lock in
              let barrier = Barrier.create (n_shared + 1) in
              for _ = 1 to n_exclusive do
                Concurrent.spawn s ~f:(fun _ _ c ->
                  Rwlock.with_access (Concurrent.await c) lock ~f:(fun _ -> ()) [@nontail])
              done;
              for _ = 1 to n_shared do
                Concurrent.spawn s ~f:(fun _ _ c ->
                  Rwlock.with_access_shared (Concurrent.await c) lock ~f:(fun _ ->
                    Barrier.await (Concurrent.await c) barrier [@nontail])
                  [@nontail])
              done;
              let shared_guard = Rwlock.Guard.downgrade guard in
              Barrier.await (Concurrent.await c) barrier;
              Rwlock.Shared_guard.release shared_guard)
            [@nontail])
          [@nontail]);
        print_s
          [%sexp
            { is_exclusive = (Rwlock.For_testing.is_exclusive lock : bool)
            ; is_shared = (Rwlock.For_testing.is_shared lock : bool)
            }]);
      [%expect
        {|
        ((is_exclusive false)
         (is_shared    false))
        |}]
    ;;
  end

  module type Lock_with_condition = sig @@ portable
    include Lock

    module Condition : sig
      type 'k lock := 'k t
      type 'k t : value mod contended portable

      val create : unit -> 'k t

      val wait
        :  Await.t @ local
        -> 'k t @ local
        -> lock:'k lock @ local
        -> 'k Capsule.Key.t @ unique
        -> 'k Capsule.Key.t @ unique

      val signal : 'k t @ local -> unit
      val broadcast : 'k t @ local -> unit
    end
  end

  module Test_with_condition (Lock : Lock_with_condition) = struct
    module Condition = Lock.Condition

    let%expect_test "basics" =
      let (P key) = Capsule.create () in
      let lock = Lock.create key in
      let condition = Condition.create () in
      S.with_concurrent ~f:(fun c ->
        List.iter [ Condition.signal; Condition.broadcast ] ~f:(fun release ->
          let n = Atomic.make 10 in
          Concurrent.with_scope c () ~f:(fun s ->
            let finished = Ivar.create () in
            let main _ _ c =
              Lock.with_key (Concurrent.await c) lock ~f:(fun key ->
                #((), Condition.wait (Concurrent.await c) condition ~lock key));
              if 1 = Atomic.fetch_and_add n (-1) then Ivar.fill_if_empty finished ()
            in
            for _ = 1 to Atomic.get n do
              Concurrent.spawn s ~f:main
            done;
            while Or_null.is_null (Ivar.peek finished) do
              Thread.yield ();
              release condition
            done))
        [@nontail])
    ;;

    let%expect_test ("stress test termination" [@tags "runtime5-only"]) =
      if Domain.recommended_domain_count () = 1
      then
        ( (* Some of the "steps" do not happen frequently enough with just threads and so we
           skip this test when we don't have domains. *) )
      else (
        (* This is a stress tests of termination with a lock and a condition variable.
         Multiple "stepper" domains repeatedly take a lock, wait on the condition, and
         record which "steps" they took.  One domain randomly terminates steppers and one
         domain signals or broadcasts the condition variable.  The test self checks that
         all the steps or paths get exercised. *)
        let (P key) = Capsule.create () in
        let lock = Lock.create key in
        let condition = Condition.create () in
        let step = Atomic.make_alone 0 in
        let step_1 = Atomic.make_alone 0 in
        let step_2 = Atomic.make_alone 0 in
        let step_3 = Atomic.make_alone 0 in
        let step_4 = Atomic.make_alone 0 in
        let step_5 = Atomic.make_alone 0 in
        let step_6 = Atomic.make_alone 0 in
        let steps = [: step_1; step_2; step_3; step_4; step_5; step_6 :] in
        let n_steppers = 4 in
        assert (Int.is_pow2 n_steppers);
        let terminators =
          Iarray.init n_steppers ~f:(fun _ -> Atomic.make_alone Terminator.never)
        in
        let attempt w i =
          let%with.stack new_terminator = Terminator.with_linked (Await.terminator w) in
          let w = Await.with_terminator w new_terminator in
          Atomic.set
            (Iarray.get terminators i)
            (Terminator.Expert.globalize new_terminator);
          Atomic.incr step_1;
          Domain.cpu_relax ();
          match
            Lock.with_key w lock ~f:(fun key ->
              let n = Atomic.get step in
              Atomic.incr step_2;
              Domain.cpu_relax ();
              Atomic.set step (n + 1);
              match Condition.wait w condition ~lock key with
              | key ->
                let n = Atomic.get step in
                Atomic.incr step_3;
                Domain.cpu_relax ();
                Atomic.set step (n + 1);
                #((), key)
              | exception exn ->
                let n = Atomic.get step in
                Atomic.incr step_4;
                Domain.cpu_relax ();
                Atomic.set step (n + 1);
                (match raise exn with
                 | (_ : Nothing.t) -> .))
          with
          | () -> Atomic.incr step_5
          | exception Await.Terminated -> Atomic.incr step_6
        in
        let exit = Atomic.make n_steppers in
        let limit = 10_000 in
        let deadline = Core_unix.gettimeofday () +. 60.0 in
        let hard_deadline = Core_unix.gettimeofday () +. 120.0 in
        let ( <. ) = Float.( <. ) in
        let main w i =
          match
            while
              Iarray.exists steps ~f:(fun step -> Atomic.get step < limit)
              && Core_unix.gettimeofday () <. deadline
            do
              attempt w i
            done
          with
          | _ -> Atomic.decr exit
          | exception exn ->
            Atomic.decr exit;
            raise exn
        in
        S.with_concurrent ~f:(fun c ->
          Concurrent.with_scope c () ~f:(fun s ->
            for i = 0 to n_steppers - 1 do
              Concurrent.spawn s ~f:(fun _ _ c -> main (Concurrent.await c) i [@nontail])
            done;
            Concurrent.spawn s ~f:(fun _ _ _ ->
              let state = Random.State.make [||] in
              while Atomic.get exit <> 0 do
                if hard_deadline <. Core_unix.gettimeofday ()
                then Core_unix.exit_immediately 4
                else Domain.cpu_relax ();
                let terminator =
                  Atomic.get
                    (Iarray.get
                       terminators
                       (Random.State.bits state land (n_steppers - 1)))
                in
                Terminator.source terminator
                |> Or_null.iter ~f:Terminator.Source.terminate
              done);
            let state = Random.State.make [||] in
            while Atomic.get exit <> 0 do
              if hard_deadline <. Core_unix.gettimeofday ()
              then Core_unix.exit_immediately 3
              else Domain.cpu_relax ();
              if Random.State.bool state
              then Condition.broadcast condition
              else Condition.signal condition
            done);
          require
            (Atomic.get step = Atomic.get step_2 + Atomic.get step_3 + Atomic.get step_4);
          if Iarray.exists steps ~f:(fun step -> Atomic.get step < limit)
          then
            steps
            |> Iarray.map ~f:(fun step -> Atomic.get step)
            |> Iarray.map ~f:(Printf.sprintf "%d")
            |> Iarray.to_list
            |> String.concat ~sep:", "
            |> Printf.printf "Lock cancelation steps: [%s]"))
    ;;

    let%expect_test "poisoning" =
      let open struct
        type state =
          | Initial
          | Waiting
          | Poisoned
      end in
      let (P key) = Capsule.create () in
      let lock = Lock.create key in
      let condition = Condition.create () in
      let state = Capsule.Data.create (fun () -> ref Initial) in
      S.with_concurrent ~f:(fun c ->
        require_does_raise (fun () ->
          Concurrent.with_scope c () ~f:(fun s ->
            let rec wait_until c ~state_is key =
              match
                Capsule.Key.access key ~f:(fun access ->
                  !(Capsule.Data.unwrap ~access state))
              with
              | #(state, key) ->
                if phys_equal state state_is
                then key
                else
                  wait_until
                    c
                    ~state_is
                    (Condition.wait (Concurrent.await c) condition ~lock key)
            in
            Concurrent.spawn s ~f:(fun _ _ c ->
              Lock.with_key (Concurrent.await c) lock ~f:(fun key ->
                let #((), key) =
                  Capsule.Key.access key ~f:(fun access ->
                    Capsule.Data.unwrap ~access state := Waiting)
                in
                Condition.signal condition;
                let key = wait_until c ~state_is:Poisoned key in
                #((), key))
              [@nontail]);
            Lock.with_key (Concurrent.await c) lock ~f:(fun key ->
              let key = wait_until c ~state_is:Waiting key in
              let key = Lock.poison lock key in
              let #((), key) =
                Capsule.Key.access key ~f:(fun access ->
                  Capsule.Data.unwrap ~access state := Poisoned)
              in
              Condition.broadcast condition;
              #((), key))
            [@nontail])
          [@nontail])
        [@nontail]);
      [%expect {| (Poisoned) |}]
    ;;
  end

  module%test Test_mutex_with_condition = Test_with_condition (Mutex)
  module%test Test_rwlock_with_condition = Test_with_condition (Rwlock)
end

module%test _ = Test_with_all_schedulers (Test_locks)
