open Base
open Await
open Expect_test_helpers_base

let require_in_range
  (type a)
  (module M : With_compare with type t = a)
  value
  ~at_least
  ~at_most
  =
  require
    (M.compare at_least value <= 0 && M.compare value at_most <= 0)
    ~if_false_then_print_s:
      (lazy
        [%message "value not in range" (value : M.t) (at_least : M.t) (at_most : M.t)])
;;

let%expect_test "Cancellation basics" =
  Cancellation.with_ (fun parent ->
    let called = Atomic.make 0 in
    let trigger = Trigger.create_with_action (fun _ -> Atomic.incr called) in
    require_equal
      (module Cancellation.Link)
      Attached
      (Cancellation.add_trigger parent (Trigger.source trigger));
    Cancellation.with_linked parent (fun child ->
      let called = Atomic.make 0 in
      let trigger = Trigger.create_with_action (fun _ -> Atomic.incr called) in
      require_equal
        (module Cancellation.Link)
        Attached
        (Cancellation.add_trigger child (Trigger.source trigger));
      Option.iter ~f:Cancellation.Source.cancel (Cancellation.source child);
      require_equal (module Int) 1 (Atomic.get called));
    require (not (Cancellation.is_canceled parent));
    require_equal (module Int) 0 (Atomic.get called);
    Cancellation.with_linked parent (fun child ->
      let called = Atomic.make 0 in
      let trigger = Trigger.create_with_action (fun _ -> Atomic.incr called) in
      require_equal
        (module Cancellation.Link)
        Attached
        (Cancellation.add_trigger child (Trigger.source trigger));
      Option.iter ~f:Cancellation.Source.cancel (Cancellation.source parent);
      require_equal (module Int) 1 (Atomic.get called));
    require (Cancellation.is_canceled parent);
    require_equal (module Int) 1 (Atomic.get called))
;;

let%expect_test "Cancellation internal cleanup" =
  Cancellation.with_ (fun t ->
    for n = 1 to 5 do
      let triggers = Array.init n ~f:(fun _ -> Trigger.source (Trigger.create ())) in
      for i = 0 to n * 4 do
        require_in_range
          (module Int)
          (Cancellation.For_testing.get_countdown t)
          ~at_least:0
          ~at_most:n;
        Trigger.Source.signal triggers.(i % n);
        triggers.(i % n) <- Trigger.source (Trigger.create ());
        require_equal
          (module Cancellation.Link)
          Attached
          (Cancellation.add_trigger t triggers.(i % n));
        require_in_range
          (module Int)
          (Cancellation.For_testing.get_countdown t)
          ~at_least:0
          ~at_most:n;
        ()
      done;
      for i = 0 to n - 1 do
        Trigger.Source.signal triggers.(i)
      done
    done;
    ())
;;
