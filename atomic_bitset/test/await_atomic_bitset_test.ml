open Base

let%expect_test _ =
  let n = 65 in
  let t = Await_atomic_bitset.create n in
  let print_t () =
    for i = 0 to n - 1 do
      Stdio.printf "%d" (Bool.to_int (Await_atomic_bitset.get t i))
    done;
    Stdio.print_endline ""
  in
  print_t ();
  [%expect {| 00000000000000000000000000000000000000000000000000000000000000000 |}];
  Await_atomic_bitset.set t 0 true;
  print_t ();
  [%expect {| 10000000000000000000000000000000000000000000000000000000000000000 |}];
  Await_atomic_bitset.set t 64 true;
  print_t ();
  [%expect {| 10000000000000000000000000000000000000000000000000000000000000001 |}];
  Await_atomic_bitset.set t 15 true;
  print_t ();
  [%expect {| 10000000000000010000000000000000000000000000000000000000000000001 |}];
  let pop () =
    match Await_atomic_bitset.non_linearizable_pop t with
    | Null -> None
    | This x -> Some x
  in
  let print_s s = Sexp.to_string_hum s |> Stdio.print_endline in
  print_s [%message (pop () : int option)];
  [%expect {| ("pop ()" (0)) |}];
  print_t ();
  [%expect {| 00000000000000010000000000000000000000000000000000000000000000001 |}];
  print_s [%message (pop () : int option)];
  [%expect {| ("pop ()" (15)) |}];
  print_t ();
  [%expect {| 00000000000000000000000000000000000000000000000000000000000000001 |}];
  print_s [%message (pop () : int option)];
  [%expect {| ("pop ()" (64)) |}];
  print_t ();
  [%expect {| 00000000000000000000000000000000000000000000000000000000000000000 |}];
  print_s [%message (pop () : int option)];
  [%expect {| ("pop ()" ()) |}];
  print_t ();
  [%expect {| 00000000000000000000000000000000000000000000000000000000000000000 |}]
;;
