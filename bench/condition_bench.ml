open Base
open Await

(** A bounded blocking queue. *)
module Bounded_queue : sig @@ portable
  type 'a t : value mod contended portable

  val create : capacity:int -> 'a t
  val is_empty : Await.t @ local -> 'a t @ local -> bool
  val push : Await.t @ local -> 'a t @ local -> 'a @ contended portable -> unit
  val pop : Await.t @ local -> 'a t @ local -> 'a @ contended portable
end = struct
  module Queue = Stdlib.Queue
  module Lock = Mutex
  module Condition = Lock.Condition

  type ('a, 'k) inner =
    { lock : 'k Lock.t
    ; queue : ('a Modes.Portended.t Queue.t, 'k) Capsule.Data.t @@ global
    ; capacity : int
    ; not_empty : 'k Condition.t
    ; not_full : 'k Condition.t
    }

  type 'a t = T : ('a, 'k) inner -> 'a t [@@unboxed]

  let create ~capacity =
    if capacity < 0
    then invalid_arg "negative capacity"
    else (
      let%tydi (P k) = Capsule.Expert.create () in
      let lock = Lock.create k in
      let queue = Capsule.Data.create Queue.create in
      let not_empty = Condition.create () in
      let not_full = Condition.create () in
      T { lock; queue; capacity; not_empty; not_full })
  ;;

  let is_empty w (T t) =
    Lock.with_access w t.lock ~f:(fun access ->
      Queue.is_empty (Capsule.Data.unwrap ~access t.queue))
    [@nontail]
  ;;

  let rec wait w t condition ~length_isnt ~was ~had_length key =
    let #(length, key) =
      Capsule.Expert.Key.access key ~f:(fun access ->
        Queue.length (Capsule.Data.unwrap ~access t.queue))
    in
    if length = length_isnt
    then (
      was := true;
      let key = Condition.wait w condition ~lock:t.lock key in
      wait w t condition ~length_isnt ~was ~had_length key)
    else (
      had_length := length;
      key)
  ;;

  let pop w (T t) =
    let was_empty = ref false in
    let had_length = ref 0 in
    let { aliased = { many = { portended = elem } } } =
      Lock.with_key w t.lock ~f:(fun key ->
        let key = wait w t t.not_empty ~length_isnt:0 ~was:was_empty ~had_length key in
        Capsule.Expert.Key.access key ~f:(fun access ->
          { aliased = { many = Queue.pop (Capsule.Data.unwrap ~access t.queue) } })
        [@nontail])
    in
    if !had_length = t.capacity then Condition.signal t.not_full;
    if !was_empty && 1 < !had_length then Condition.signal t.not_empty;
    elem
  ;;

  let push w (T t) x =
    let was_full = ref false in
    let had_length = ref 0 in
    Lock.with_key w t.lock ~f:(fun key ->
      let key =
        wait w t t.not_full ~length_isnt:t.capacity ~was:was_full ~had_length key
      in
      Capsule.Expert.Key.access key ~f:(fun access ->
        Queue.push { portended = x } (Capsule.Data.unwrap ~access t.queue))
      [@nontail]);
    if !had_length = 0 then Condition.signal t.not_empty;
    if !was_full && !had_length < t.capacity - 1 then Condition.signal t.not_full
  ;;
end

module Config = struct
  let decode config =
    let n_pushers = config / 10 in
    let n_poppers = config % 10 in
    let n_threads = n_pushers + n_poppers in
    ~n_pushers, ~n_poppers, ~n_threads
  ;;

  let configs =
    [ 11; 12; 21; 22; 14; 41; 44 ]
    |> List.filter ~f:(fun config ->
      let ~n_pushers:_, ~n_poppers:_, ~n_threads = decode config in
      n_threads <= Domain.recommended_domain_count ())
  ;;
end

let%bench_fun ("Blocking_queue" [@indexed config = Config.configs]) =
  let ~n_pushers, ~n_poppers, ~n_threads = Config.decode config in
  let n_messages = 100_000 in
  let t =
    (* Intentionally small [capacity], which will likely cause some [push]es to block. *)
    Bounded_queue.create ~capacity:10
  in
  fun () ->
    let%with.tilde.stack c = Concurrent_in_thread.with_concurrent Terminator.never in
    let barrier = Barrier.create n_threads in
    Concurrent.with_scope c () ~f:(fun s ->
      for i = 1 to n_threads do
        Concurrent.spawn s ~f:(fun _ _ c ->
          let w = Concurrent.await c in
          if i <= n_poppers
          then (
            Barrier.await w barrier;
            for _ = 1 to n_messages / n_poppers do
              let _ : _ = Bounded_queue.pop w t in
              ()
            done)
          else (
            Barrier.await w barrier;
            for i = 1 to n_messages / n_pushers do
              (* We intentionally use non-immediate messages.*)
              Bounded_queue.push w t (Some i)
            done))
      done);
    assert (Bounded_queue.is_empty (Concurrent.await c) t)
;;
