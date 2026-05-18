open! Base
open! Import

type 'a t =
  { queue : 'a aliased_many Portable_mpmc_queue.t
  ; awaitable : int Awaitable.t
  }

let create ?padded () =
  let awaitable = Awaitable.make ?padded 0 in
  let queue = Portable_mpmc_queue.create ?padded () in
  { queue; awaitable }
;;

let push t a =
  Portable_mpmc_queue.push t.queue { aliased_many = a };
  Awaitable.incr t.awaitable;
  Awaitable.signal t.awaitable
;;

type state =
  | Never_awaited
  | Signaled

let pop await (t @ local) =
  let[@inline] rec loop state =
    match Portable_mpmc_queue.pop t.queue with
    | This v ->
      (match state with
       | Signaled -> Awaitable.signal t.awaitable
       | Never_awaited -> ());
      v
    | Null ->
      let before = Awaitable.get t.awaitable in
      (match Portable_mpmc_queue.pop t.queue with
       | This v ->
         (match state with
          | Signaled -> Awaitable.signal t.awaitable
          | Never_awaited -> ());
         v
       | Null ->
         (match Awaitable.await await t.awaitable ~until_phys_unequal_to:before with
          | Terminated -> raise Await.Terminated
          | Signaled -> loop Signaled))
  in
  (loop Never_awaited).aliased_many
;;

let pop_or_cancel await c t : _ Or_canceled.t =
  let[@inline] rec loop state : _ Or_canceled.t =
    match Portable_mpmc_queue.pop t.queue with
    | This v ->
      (match state with
       | Signaled -> Awaitable.signal t.awaitable
       | Never_awaited -> ());
      Completed v
    | Null ->
      let before = Awaitable.get t.awaitable in
      (match Portable_mpmc_queue.pop t.queue with
       | This v ->
         (match state with
          | Signaled -> Awaitable.signal t.awaitable
          | Never_awaited -> ());
         Completed v
       | Null ->
         (match
            Awaitable.await_or_cancel await c t.awaitable ~until_phys_unequal_to:before
          with
          | Terminated -> raise Await.Terminated
          | Canceled -> Canceled
          | Signaled -> loop Signaled))
  in
  match loop Never_awaited with
  | Completed { aliased_many } -> Completed aliased_many
  | Canceled -> Canceled
;;

let pop_nonblocking t =
  match Portable_mpmc_queue.pop t.queue with
  | This { aliased_many } -> This aliased_many
  | Null -> Null
;;

let peek t =
  match Portable_mpmc_queue.peek t.queue with
  | This { aliased_many } -> This aliased_many
  | Null -> Null
;;

let[@inline] length t = Portable_mpmc_queue.length t.queue

module For_testing = struct
  let length t = Awaitable.For_testing.length t.awaitable
end
