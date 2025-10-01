open! Core
open Import
open Await
open Expect_test_helpers_core

let gen_list_with_length_range ~lo ~hi quickcheck_generator_a =
  let open Base_quickcheck.Generator in
  let open Let_syntax in
  let%bind length = Int.gen_incl lo hi in
  list_with_length quickcheck_generator_a ~length
;;

let%expect_test "Mvar basics - single threaded" =
  let t = Mvar.create () in
  print_s [%sexp (Mvar.is_full t : bool)];
  [%expect {| false |}];
  Await.For_testing.with_never ~f:(fun w -> Mvar.put w t "value");
  print_s [%sexp (Mvar.is_full t : bool)];
  [%expect {| true |}];
  let value = Mvar.try_take t in
  print_s [%sexp (value : string or_null)];
  [%expect {| (value) |}];
  print_s [%sexp (Mvar.is_full t : bool)];
  [%expect {| false |}];
  Await.For_testing.with_never ~f:(fun w ->
    Mvar.put w t "another value";
    let value = Mvar.take w t in
    print_s [%sexp (value : string)];
    [%expect {| "another value" |}])
;;

module Test (S : S) = struct
  let%quick_test ("many threads putting and taking" [@trials 3]) =
    fun (putters :
          (int list list
          [@generator
            gen_list_with_length_range ~lo:0 ~hi:10 [%quickcheck.generator: int list]]))
      (num_takers : (int[@generator Int.gen_incl 0 10])) ->
    let timeout_sec = 10 in
    let%with.tilde conc = S.with_concurrent in
    let%with.stack c = Cancellation.with_ in
    let t = Mvar.create () in
    let results = Atomic.make [] in
    let timed_out = Atomic.make false in
    Concurrent.with_scope conc (Cancellation.Expert.globalize c) ~f:(fun s ->
      let running = Atomic.make 0 in
      (* Run a task to time out after a period of time *)
      Concurrent.spawn s ~f:(fun s _ _ ->
        Core_unix.sleep timeout_sec;
        Cancellation.Source.cancel
          (s |> Scope.context |> Cancellation.source |> Or_null.value_exn);
        if Atomic.get running > 0 then Atomic.set timed_out true);
      let putters_done =
        Barrier.create (List.length putters + 1 (* + 1 for us, so we can wait too *))
      in
      List.iter putters ~f:(fun values_to_put ->
        Concurrent.spawn s ~f:(fun s _ conc ->
          Atomic.incr running;
          let[@inline] rec go : int list -> _ Or_canceled.t = function
            | v :: vs ->
              (match Mvar.put_or_cancel (Concurrent.await conc) (Scope.context s) t v with
               | Canceled -> Canceled
               | Completed () -> go vs)
            | [] -> Completed ()
          in
          ignore (go values_to_put : unit Or_canceled.t);
          Atomic.decr running;
          Barrier.await (Concurrent.await conc) putters_done [@nontail]));
      for _ = 0 to num_takers do
        Concurrent.spawn s ~f:(fun s _ conc ->
          Atomic.incr running;
          let[@inline] rec go () =
            match Mvar.take_or_cancel (Concurrent.await conc) (Scope.context s) t with
            | Completed v ->
              Atomic.update results ~pure_f:(fun vs -> v :: vs);
              go ()
            | Canceled -> ()
          in
          go ();
          Atomic.decr running)
      done;
      Barrier.await (Concurrent.await conc) putters_done;
      Cancellation.Source.cancel (Cancellation.source c |> Or_null.value_exn));
    require_equal
      (module struct
        type t = int list [@@deriving sexp_of]

        let equal t1 t2 =
          [%equal: int list]
            (List.sort ~compare:[%compare: int] t1)
            (List.sort ~compare:[%compare: int] t2)
        ;;
      end)
      (Atomic.get results)
      (List.concat putters)
  ;;
end

module%test _ = Test_with_all_schedulers (Test)
