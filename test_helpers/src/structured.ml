open Base
open Portable
open Await

module Scope = struct
  type 'a t =
    { live : int Atomic.t @@ global (** The number of currently live threads *)
    ; finished : Trigger.t @@ global
    ; failure : (exn * Backtrace.t) option Atomic.t @@ global
    ; terminator : Await.Terminator.t @@ global
    ; context : 'a @@ contended global portable
    }

  let raised_global ~failure ~terminator exn bt =
    (* NOTE: Making the [exn] and [bt] magically portable is unsafe in some cases (eg with
       nonportable scopes). But (we currently believe) this requires actively malicious
       use of exceptions to smuggle data out of capsules, soon, we will require that all
       exceptions are portable, so we are leaving this magic here for now. *)
    let exn = Stdlib.Obj.magic_portable exn in
    let bt = Stdlib.Obj.magic_portable bt in
    match exn with
    | Terminated -> ()
    | exn ->
      (match Atomic.get failure with
       | None ->
         let _ : _ option =
           Atomic.compare_exchange
             failure
             ~if_phys_equal_to:None
             ~replace_with:(Some (exn, bt))
         in
         Option.iter ~f:Terminator.Source.terminate (Terminator.source terminator)
       | Some _ -> ())
  ;;

  let[@inline] raised { failure; terminator; _ } exn bt =
    raised_global ~failure ~terminator exn bt
  ;;

  let incr ~live =
    let prior = Atomic.fetch_and_add live 1 in
    (* As the scope is local and is not passed to children, the [live] count should always
       be strictly positive when [incr] is called.  If the scope would be alllowed to
       escape, [incr] should be changed to check that the count is not [0] before
       [compare_and_set] to increment it. *)
    assert%debug (0 < prior)
  ;;

  let%template decr ~live ~finished =
    let prior = Atomic.fetch_and_add live (-1) in
    assert%debug (1 <= prior);
    if prior = 1
    then Trigger.Source.signal ((Trigger.source [@mode local]) finished) [@nontail]
  ;;

  let%template add ~spawn ~setup { live; finished; failure; terminator; context } ~f =
    incr ~live;
    try
      (* The assumption here is that when [spawn] returns normally, the thunk given to it
         will be called, and when [spawn] raises, the thunk will not be called. *)
      spawn (fun () ->
        (* [setup] sets up whatever handlers are required to run the thread of control. *)
        setup terminator ~f:(fun w ->
          (try f w context with
           | exn ->
             let bt = Backtrace.Exn.most_recent () in
             raised_global ~failure ~terminator exn bt);
          (* [decr] must be performed inside the [setup] as [setup] may return before the
             thread of control is done. *)
          decr ~live ~finished))
    with
    | exn ->
      let bt = Backtrace.Exn.most_recent () in
      decr ~live ~finished;
      Exn.raise_with_original_backtrace exn bt
  [@@mode portability = (nonportable, portable), locality = (global, local)]
  ;;

  let create terminator context = exclave_
    { live = Atomic.make 1 (* Includes the thread of control that created the scope. *)
    ; finished = Trigger.create ()
    ; failure = Atomic.make None
    ; terminator = Terminator.Expert.globalize terminator
    ; context
    }
  ;;

  let finish w { live; finished; failure; _ } =
    decr ~live ~finished;
    await_never_terminated w finished;
    match Atomic.get failure with
    | None -> ()
    | Some (exn, bt) ->
      (* See comment in [raised_global] about safety. *)
      let exn = Stdlib.Obj.magic_uncontended exn in
      let bt = Stdlib.Obj.magic_uncontended bt in
      Exn.raise_with_original_backtrace exn bt
  ;;
end

let with_scope w context ~f =
  Terminator.with_linked (terminator w) (fun terminator ->
    let s = Scope.create terminator context in
    match f (Await.with_terminator w terminator) s with
    | result ->
      Scope.finish w s;
      result
    | exception exn ->
      let bt = Backtrace.Exn.most_recent () in
      Scope.raised s exn bt;
      Scope.finish w s;
      Exn.raise_with_original_backtrace exn bt)
  [@nontail]
;;
