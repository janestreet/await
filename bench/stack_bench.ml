open Base
open Await

module Kind = struct
  type t =
    | Blocking
    | Cancellable
    | Nonblocking

  let to_label = function
    | Blocking -> "b"
    | Cancellable -> "o"
    | Nonblocking -> "n"
  ;;
end

let params =
  let n_threads = [ 1; 2; 4 ] in
  List.concat_map [ Kind.Blocking; Cancellable; Nonblocking ] ~f:(fun consumer_kind ->
    List.concat_map n_threads ~f:(fun n_producers ->
      List.concat_map n_threads ~f:(fun n_consumers ->
        if n_producers + n_consumers <= Domain.recommended_domain_count ()
        then (
          let label =
            Printf.sprintf
              "%dbp %d%sc"
              n_producers
              n_consumers
              (Kind.to_label consumer_kind)
          in
          [ label, (~n_producers, ~n_consumers, ~consumer_kind) ])
        else [])))
;;

let%bench_fun ("Stack" [@params params = params]) =
  let ~(n_producers : int), ~(n_consumers : int), ~(consumer_kind : Kind.t) = params in
  let n_domains = n_producers + n_consumers in
  let n_msgs = 1_000_000 in
  let n_msgs_to_prod_per_domain = n_msgs / n_producers in
  let n_msgs_to_cons_per_domain = n_msgs / n_consumers in
  let t = Stack.create () in
  fun () ->
    let%with.tilde.stack c = Concurrent_in_thread.with_concurrent Terminator.never in
    let barrier = Barrier.create n_domains in
    let work c i =
      Barrier.await (Concurrent.await c) barrier;
      if i < n_producers
      then
        for i = 1 to n_msgs_to_prod_per_domain do
          Stack.push t (Some i)
        done
      else (
        match consumer_kind with
        | Nonblocking ->
          let rec loop n =
            if 0 < n
            then (
              match Stack.pop_nonblocking t with
              | Null -> loop n
              | This _ -> loop (n - 1))
          in
          loop n_msgs_to_cons_per_domain
        | Blocking ->
          for _ = 1 to n_msgs_to_cons_per_domain do
            let _ : _ = Stack.pop (Concurrent.await c) t in
            ()
          done
        | Cancellable ->
          let%with.stack cancel = Cancellation.with_ in
          for _ = 1 to n_msgs_to_cons_per_domain do
            let _ : _ = Stack.pop_or_cancel (Concurrent.await c) cancel t in
            ()
          done)
    in
    Concurrent.with_scope c () ~f:(fun s ->
      for i = 1 to n_domains - 1 do
        Concurrent.spawn s ~f:(fun _ _ c -> work c i)
      done;
      work c 0);
    [%test_eq: int] (List.length (Stack.drain t)) 0
;;
