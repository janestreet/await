open Base
open Await
open Expect_test_helpers_base

let%expect_test "Trigger basics" =
  let () =
    let trigger = Trigger.create () in
    require (not (Trigger.is_signalled trigger));
    let called = Atomic.make 0 in
    require (Trigger.on_signal trigger (fun _ -> Atomic.incr called));
    require (not (Trigger.is_signalled trigger));
    Trigger.Source.signal (Trigger.source trigger);
    require (Trigger.is_signalled trigger);
    require_equal (module Int) 1 (Atomic.get called)
  in
  let called = Atomic.make 0 in
  let trigger = Trigger.create_with_action (fun _ -> Atomic.incr called) in
  require (Trigger.drop trigger);
  require (Trigger.is_signalled trigger);
  require_equal (module Int) 0 (Atomic.get called)
;;
