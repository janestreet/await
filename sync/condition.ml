open! Base
open! Import
include Condition_intf

module Make (Lock : Arg) = struct
  module Wait = struct
    type 'k t =
      { lock : 'k Lock.t
      ; mutable lock_is_held : bool
      ; await : Await.t
      }

    let%template[@inline] release_temporarily cw k ~f =
      (let { lock; await; _ } = cw in
       Lock.unsafe_release lock;
       cw.lock_is_held <- false;
       let res = f await in
       if Await.is_terminated await
       then (
         match raise Await.Terminated with
         | (_ : Nothing.t) -> .)
       else (
         Lock.unsafe_acquire await lock;
         (* We only set this to true here once we know we haven't been terminated, and
            hence can return the lock back to the user *)
         cw.lock_is_held <- true;
         #(res, k)))
      [@exclave_if_local l ~reasons:[ May_return_local ]]
    [@@mode l = (global, local)]
    ;;
  end

  let lock_is_held { Wait.lock_is_held; _ } = lock_is_held

  let%template[@inline] with_wait await lock (key : _ Capsule.Key.t) f =
    f
      { Wait.lock
      ; await
      ; lock_is_held =
          (* We know the lock is held; we take the key as an argument to prove it *)
          true
      }
      key [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]]
  [@@mode l = (global, local)]
  ;;

  type 'k t = bool Awaitable.t

  let create ?padded () = Awaitable.make ?padded true

  let[@inline] wait cw t k =
    let { Wait.lock; await; _ } = cw in
    let trigger = Trigger.create () in
    let awaiter =
      (Awaitable.Awaiter.(create_and_add [@mode local]) t)
        (Trigger.source trigger)
        ~until_phys_unequal_to:false
    in
    Lock.unsafe_release lock;
    cw.lock_is_held <- false;
    Await.await_until_terminated await trigger;
    if Await.is_terminated await
    then (
      Awaitable.Awaiter.cancel_and_remove awaiter;
      Awaitable.signal t;
      match raise Await.Terminated with
      | (_ : Nothing.t) -> .)
    else (
      match Lock.unsafe_acquire await lock with
      | exception (Await.Terminated as exn) ->
        (* It might be the case that, eg, waiting on a condition variable is not actually
           terminated, but then the terminator gets terminated before we get to re-acquire
           the lock. In that case, we need to make sure to re-signal the condition
           variable to avoid dropping the signal (which could cause a deadlock). *)
        let bt = Backtrace.Exn.most_recent () in
        Awaitable.signal t;
        (match Exn.raise_with_original_backtrace exn bt with
         | (_ : Nothing.t) -> .)
      | () ->
        cw.lock_is_held <- true;
        k)
  ;;

  let signal = Awaitable.signal
  let broadcast = Awaitable.broadcast
end
