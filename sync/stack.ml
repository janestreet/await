open! Base
open! Import

(** See [Adaptive_backoff.once] *)
let log_scale = 11

type 'a t = 'a Modes.Portended.t list Awaitable.t

let create ?padded () = Awaitable.make ?padded []

let sexp_of_t (type a : value mod contended) sexp_of_a (t : a t) =
  Awaitable.get t |> List.map ~f:(fun { portended } -> portended) |> [%sexp_of: a list]
;;

let push t a =
  let[@inline] rec loop () =
    let before = Awaitable.get t in
    match
      Awaitable.compare_and_set
        t
        ~if_phys_equal_to:before
        ~replace_with:({ portended = a } :: before)
    with
    | Set_here ->
      (match before with
       | [] -> Awaitable.signal t
       | _ :: _ -> ())
    | Compare_failed ->
      Adaptive_backoff.once ~random_key:(Awaitable.random_key t) ~log_scale;
      loop ()
  in
  loop () [@nontail]
;;

type state =
  | Never_awaited
  | Signaled

let pop await t =
  let[@inline] rec loop state =
    match Awaitable.get t with
    | [] ->
      (match Awaitable.await await t ~until_phys_unequal_to:[] with
       | Terminated -> raise Await.Terminated
       | Signaled -> loop Signaled)
    | x :: xs as cur ->
      (match Awaitable.compare_and_set t ~if_phys_equal_to:cur ~replace_with:xs with
       | Set_here ->
         (* Signal another awaiter in case we consumed a signal and there are items. *)
         (match state, xs with
          | Signaled, _ :: _ -> Awaitable.signal t
          | Never_awaited, _ | _, [] -> ());
         x.portended
       | Compare_failed ->
         Adaptive_backoff.once ~random_key:(Awaitable.random_key t) ~log_scale;
         loop state)
  in
  loop Never_awaited [@nontail]
;;

let pop_or_cancel await c t =
  let[@inline] rec loop state : _ Or_canceled.t =
    match Awaitable.get t with
    | [] ->
      (match Awaitable.await_or_cancel await c t ~until_phys_unequal_to:[] with
       | Terminated -> raise Await.Terminated
       | Canceled -> Canceled
       | Signaled -> loop Signaled)
    | x :: xs as cur ->
      (match Awaitable.compare_and_set t ~if_phys_equal_to:cur ~replace_with:xs with
       | Set_here ->
         (* Signal another awaiter in case we consumed a signal and there are items. *)
         (match state, xs with
          | Signaled, _ :: _ -> Awaitable.signal t
          | Never_awaited, _ | _, [] -> ());
         Completed x.portended
       | Compare_failed ->
         Adaptive_backoff.once ~random_key:(Awaitable.random_key t) ~log_scale;
         loop state)
  in
  loop Never_awaited [@nontail]
;;

let pop_nonblocking t =
  let[@inline] rec loop () =
    match Awaitable.get t with
    | [] ->
      Adaptive_backoff.once_unless_alone ~random_key:(Awaitable.random_key t) ~log_scale;
      Null
    | x :: xs as cur ->
      (match Awaitable.compare_and_set t ~if_phys_equal_to:cur ~replace_with:xs with
       | Set_here -> This x.portended
       | Compare_failed ->
         Adaptive_backoff.once ~random_key:(Awaitable.random_key t) ~log_scale;
         loop ())
  in
  loop () [@nontail]
;;

external portended_list
  :  'a Modes.Portended.t list
  -> 'a list @ contended portable
  @@ portable
  = "%identity"

let drain t = Awaitable.exchange t [] |> portended_list

let drain_blocking await t =
  let[@inline] rec loop () =
    match Awaitable.get t with
    | [] ->
      (match Awaitable.await await t ~until_phys_unequal_to:[] with
       | Terminated -> raise Await.Terminated
       | Signaled -> loop ())
    | _ :: _ ->
      (match Awaitable.exchange t [] with
       | [] ->
         Adaptive_backoff.once ~random_key:(Awaitable.random_key t) ~log_scale;
         loop ()
       | _ :: _ as list -> portended_list list)
  in
  loop () [@nontail]
;;

let drain_blocking_or_cancel await c t =
  let[@inline] rec loop () =
    match Awaitable.get t with
    | [] ->
      (match Awaitable.await_or_cancel await c t ~until_phys_unequal_to:[] with
       | Terminated -> raise Await.Terminated
       | Canceled -> Or_canceled.Canceled
       | Signaled -> loop ())
    | _ :: _ ->
      (match Awaitable.exchange t [] with
       | [] ->
         Adaptive_backoff.once ~random_key:(Awaitable.random_key t) ~log_scale;
         loop ()
       | _ :: _ as list -> Completed (portended_list list))
  in
  loop () [@nontail]
;;

module For_testing = Awaitable.For_testing
