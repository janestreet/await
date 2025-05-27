open Base
open Portable
open Await
open Await_test_helpers

let require_equal expected actual =
  if expected <> actual
  then Stdio.printf "Error: expected=%d actual=%d\n%!" expected actual
;;

let spawn = Multicore.spawn
and setup = Await_blocking.with_await

let%expect_test "Structured basics" =
  (* This tests that the scope waits for children even when the number of children
     momentarily goes down to zero during the execution of the scope body. *)
  let all_done = Atomic.make 0 in
  setup Terminator.never ~f:(fun w ->
    let value = 101 in
    Structured.with_scope w value ~f:(fun w s ->
      for _ = 1 to 3 do
        let trigger = Trigger.create () in
        Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun _w v ->
          require_equal value v;
          Trigger.Source.signal (Trigger.source trigger));
        await_until_terminated w trigger;
        (* Wait a bit to make it likely that no child is a alive. *)
        Thread.delay 0.01
      done;
      Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun _ _ ->
        (* Wait a bit to make it likely that child is alive when the scope body returns. *)
        Thread.delay 0.01;
        Atomic.set all_done 1));
    require_equal 1 (Atomic.get all_done));
  Stdio.print_endline "Done";
  [%expect {| Done |}]
;;

let%expect_test "demonstrate soundness of portable exceptions" =
  let open struct
    exception Smuggler of (unit -> int)
  end in
  (try
     setup Terminator.never ~f:(fun w ->
       Structured.with_scope w () ~f:(fun _w s ->
         Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun _w () ->
           let r = ref 0 in
           raise
             (Smuggler
                (fun () ->
                  Int.incr r;
                  !r)))))
   with
   | Smuggler f ->
     (* At this point only a single thread has access to [f] so this is entirely safe. *)
     Stdio.print_s [%sexp (f () : int)]);
  [%expect {| 1 |}]
;;
