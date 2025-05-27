[@@@alert "-unsafe_parallelism"]

open Base
open Portable
open Await
open Await_blocking
module Thread = Caml_threads.Thread

type request : value mod contended portable =
  { action : unit -> unit @@ portable
  ; mutable error : (exn * Backtrace.t) Modes.Portable.t option
  ; ready : Trigger.t
  }
[@@unsafe_allow_any_mode_crossing
  (* The mutable [error] field is synchronized via the [ready] trigger. *)]

type t : value mod contended portable =
  { index : int
  ; threads : int Atomic.t
  ; incoming : request list Atomic.t
  (** Closures which have been requested to be run on this domain *)
  ; mutable domain : unit Domain.t Uopt.t
  (** Handle to the underlying domain, if one is running. *)
  ; mutex : Stdlib.Mutex.t
  ; condition : Stdlib.Condition.t
  }
[@@unsafe_allow_any_mode_crossing
  (* The mutable [domain] field is synchronized via the [threads] atomic. *)]

let pause_manager t =
  Stdlib.Mutex.lock t.mutex;
  if 1 < Atomic.get t.threads && phys_equal [] (Atomic.get t.incoming)
  then Stdlib.Condition.wait t.condition t.mutex;
  Stdlib.Mutex.unlock t.mutex
;;

let wakeup_manager t =
  Stdlib.Mutex.lock t.mutex;
  Stdlib.Mutex.unlock t.mutex;
  Stdlib.Condition.broadcast t.condition
;;

let domain_key = Domain.Safe.DLS.new_key (fun () -> -1)

let () =
  (* We must set the value for the initial domain, because [current_domain] might be
     called before the manager thread for the initial domain has started. *)
  Domain.Safe.DLS.access (fun access -> Domain.Safe.DLS.set access domain_key 0)
;;

let current_domain () =
  let i = Domain.Safe.DLS.access (fun access -> Domain.Safe.DLS.get access domain_key) in
  if 0 <= i
  then i
  else invalid_arg "Multicore.current_domain: not called from a managed domain"
;;

let domains =
  Iarray.init
    (if Basement.Stdlib_shim.runtime5 () then Domain.recommended_domain_count () else 1)
    ~f:(fun i ->
      { index = i
      ; threads = Atomic.make (Bool.to_int (i = 0) * 2)
      ; incoming = Atomic.make []
      ; domain = Uopt.none
      ; mutex = Stdlib.Mutex.create ()
      ; condition = Stdlib.Condition.create ()
      })
;;

let[@inline] max_domains () = Iarray.length domains

let get i =
  assert%debug (0 <= i && i < max_domains ());
  Iarray.unsafe_get domains i
;;

let push stack x = Atomic.update stack ~pure_f:(fun s -> x :: s)

(* A set of domains which are not running a thread *)
let idle_domains = Await_atomic_bitset.create (max_domains ())

let () =
  for i = 1 to max_domains () - 1 do
    Await_atomic_bitset.set idle_domains i true
  done
;;

(** Run some function on a new thread. *)
let thread action =
  let decr () =
    let t = get (current_domain ()) in
    let threads_before_decr = Atomic.fetch_and_add t.threads (-1) in
    if threads_before_decr = 2
    then
      (* Only the manager thread was running on the domain in addition to us.  We must
         wakeup the manager to potentially allow it to exit. *)
      wakeup_manager t
  in
  match action () with
  | () -> decr ()
  | exception exn ->
    let bt = Backtrace.Exn.most_recent () in
    (* We catch unhandled exceptions in order to adjust the number of running threads. *)
    decr ();
    Exn.raise_with_original_backtrace exn bt
;;

(* The manager thread, one of which runs per domain *)
let rec manager_loop t =
  let threads = Atomic.get t.threads in
  if threads = 1
  then (
    (* We are the only thread running on the domain so we try to exit.  This is not a
       pool. *)
    match
      Atomic.compare_and_set
        t.threads
        ~if_phys_equal_to:threads
        ~replace_with:(threads - 1)
    with
    | Set_here -> Await_atomic_bitset.set idle_domains t.index true
    | Compare_failed -> manager_loop t)
  else (
    match Atomic.get t.incoming with
    | [] ->
      pause_manager t;
      manager_loop t
    | _ ->
      let requests = Atomic.exchange t.incoming [] in
      List.iter requests ~f:(fun request ->
        (match Thread.Portable.create thread request.action with
         | _ -> ()
         | exception exn ->
           (* This might fail if the user tries to create too many threads *)
           let bt = Backtrace.Exn.most_recent () in
           (* The only exception raised by [Thread.Portable.create] is [Sys_error of
              string] which is immutable data and we only use the [exn] and [bt] to
              reraise to the caller of [spawn_on] in that function.  In other words, the
              [exn] and [bt] are in fact portable and uncontended meaning that they do not
              contain shared mutable state being potentially accessed by multiple
              threads. *)
           request.error <- Some { portable = Stdlib.Obj.magic_portable (exn, bt) });
        Trigger.Source.signal (Trigger.source request.ready));
      manager_loop t)
;;

let manager t =
  Await_atomic_bitset.set idle_domains t.index false;
  Domain.Safe.DLS.access (fun access -> Domain.Safe.DLS.set access domain_key t.index);
  manager_loop t
;;

let _manager_for_initial_domain : Thread.t = Thread.Portable.create manager (get 0)

let spawn_on ~domain:i f =
  if i < 0 || max_domains () <= i
  then invalid_arg "Multicore.spawn_on: invalid domain index";
  let f =
    let open struct
      external magic_many
        :  'a @ once portable
        -> 'a @ many portable
        @@ portable
        = "%identity"
    end in
    magic_many f
  in
  let t = get i in
  let threads_before_incr = Atomic.fetch_and_add t.threads 1 in
  let request = { action = f; error = None; ready = Trigger.create () } in
  push t.incoming request;
  if threads_before_incr = 0
  then (
    (* At this point it is our responsibility to spawn a domain to run the manager thread
       on it.

       We increment threads by two -- one for the manager thread and one to prevent the
       manager thread from exiting before we have stored the domain handle of the manager
       thread. *)
    let _ : int = Atomic.fetch_and_add t.threads 2 in
    let old = t.domain in
    (* We must join with the previous manager domain, if any.  Otherwise it would be
       possible to attempt to start too many domains. *)
    if Uopt.is_some old then Domain.join (Uopt.unsafe_value old);
    t.domain <- Uopt.some (Domain.Safe.spawn (fun () -> manager t));
    (* We have successfully stored the domain handle and now decrement [threads] to allow
       the manager thread to exit. *)
    Atomic.decr t.threads);
  (* We have added incoming work and must wakeup the manager thread. *)
  wakeup_manager t;
  with_await Terminator.never ~f:(fun w -> await_until_terminated w request.ready);
  match request.error with
  | None -> ()
  | Some { portable = exn, bt } -> Exn.raise_with_original_backtrace exn bt
;;

let spawn f =
  let i =
    (* We first try and see if there are idle domains. *)
    match Await_atomic_bitset.non_linearizable_pop idle_domains with
    | This i -> i
    | Null ->
      (* Instead of expensively maintaining a priority queue, for example, we take random
         samples from the domains and pick the domain that has fewer threads running on
         it. *)
      let x = Random.int (max_domains ()) in
      let y = Random.int (max_domains ()) in
      let i = if Atomic.get (get x).threads < Atomic.get (get y).threads then x else y in
      (* We check again if there are idle domains. *)
      (match Await_atomic_bitset.non_linearizable_pop idle_domains with
       | Null ->
         (* Note that we don't really care about cases where [non_linearizable_pop] might
            return [Null] even when there is a bit set.  We just want to make a good
            enough decision on which domain to spawn to. *)
         i
       | This i -> i)
  in
  spawn_on ~domain:i f
;;
