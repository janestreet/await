open Base
open Import
open Await
open Expect_test_helpers_base

let%expect_test "Atom.(make|get|update)" =
  let x = Atom.make 101 in
  require_equal (module Int) 101 (Atom.get x);
  require_equal (module Int) 101 (Atom.update_and_return x ~pure_f:(fun x -> x - 59));
  require_equal (module Int) 42 (Atom.get x);
  [%expect {| |}]
;;

module Test (S : S) = struct
  let%expect_test "Atom.wait" =
    S.with_concurrent ~f:(fun c ->
      let x = Atom.make 1 in
      Concurrent.with_scope c () ~f:(fun s ->
        Concurrent.spawn s ~f:(fun _ _ c ->
          Atom.wait (Concurrent.await c) x ~until_phys_unequal_to:1;
          require_equal (module Int) 2 (Atom.update_and_return x ~pure_f:(fun x -> x + 1));
          Atom.wait (Concurrent.await c) x ~until_phys_unequal_to:3;
          require_equal (module Int) 4 (Atom.update_and_return x ~pure_f:(fun x -> x + 1));
          ());
        require_equal (module Int) 1 (Atom.update_and_return x ~pure_f:(fun x -> x + 1));
        Atom.wait (Concurrent.await c) x ~until_phys_unequal_to:2;
        require_equal (module Int) 3 (Atom.update_and_return x ~pure_f:(fun x -> x + 1));
        ());
      require_equal (module Int) 5 (Atom.get x));
    [%expect {| |}]
  ;;

  let%expect_test "Atom.update exception poisons atom" =
    S.with_await ~f:(fun w ->
      let x = Atom.make 42 in
      require_does_raise (fun () -> Atom.update x ~pure_f:(fun _ -> invalid_arg "_"));
      require_equal (module Int) 42 (Atom.get x);
      require_does_raise (fun () -> Atom.wait w x ~until_phys_unequal_to:42);
      require_equal (module Int) 42 (Atom.get x);
      require_does_raise (fun () -> Atom.update x ~pure_f:(fun _ -> 101));
      require_equal (module Int) 42 (Atom.get x));
    [%expect
      {|
      (Invalid_argument _)
      (Poisoned)
      (Poisoned)
      |}]
  ;;

  let%expect_test "Atom.update avoids starvation" =
    S.with_concurrent ~f:(fun c ->
      Concurrent.with_scope c () ~f:(fun s ->
        let n_interference = 3 in
        let n_interference_started = Atom.make 0 in
        let x = Atom.make 1 in
        for _ = 1 to n_interference do
          Concurrent.spawn s ~f:(fun _ _ _ ->
            let incr x = x + 1 in
            Atom.update n_interference_started ~pure_f:(fun n -> n + 1);
            while Atom.get x > 0 do
              Atom.update x ~pure_f:incr
            done)
        done;
        Atom.wait_for (Concurrent.await c) n_interference_started ~f:(fun n ->
          n = n_interference);
        print_endline "Attempting very slow update.";
        Atom.update x ~pure_f:(fun _ ->
          let _ : float = Core_unix.nanosleep 0.1 in
          -10))
      [@nontail]);
    [%expect {| Attempting very slow update. |}]
  ;;
end

module%test _ = Test_with_all_schedulers (Test)
