open Base
open Await_kernel
include Await_sync_intf
include Lazy_intf

module Expert = struct
  module Make (C : Lock_common_intf.Capability) = struct
    (* The underlying state machine of a lazy:

       {v
                                               [from_val]-----+----> Computed
                                                              |
         [from_fun] -> Uncomputed --[force]--> Computing --+--+
                                       |                   |  |
                                       +-----> Awaiters----+  +----> Error
       v}

       The [Computed] and [Error] states are terminal. *)

    type ('a : value_or_null) state =
      | Uncomputed of
          (#(C.t * 'a t global) @ local -> 'a @ contended portable) @@ portable
      | Computing
      | Awaiters
      | Computed of 'a @@ contended portable
      | Error of exn

    and ('a : value_or_null) t = 'a state Awaitable.t

    let from_val ?padded value = Awaitable.make ?padded (Computed value)

    (* We manually enforce that the thunk stored inside an [Uncomputed] lazy is only
       called once, by only exposing an API that allows each function to be called once,
       and have manually verified (via careful code review and tests) that all possible
       thread interleavings of calls to [force] only call the function once. This property
       cannot be enforced by the type system - so we have to use magic here to assert that
       the function cannot be called more than once. *)
    external magic_many_lazy_thunk
      :  'a @ once portable
      -> 'a @ many portable
      @@ portable
      = "%identity"

    let from_fun_fixed ?padded f =
      Awaitable.make
        ?padded
        (Uncomputed (magic_many_lazy_thunk (fun #(cap, { global = t }) -> f cap t)))
    ;;

    let from_fun ?padded f = from_fun_fixed ?padded (fun cap _ -> f cap) [@tail]

    type ('a : value_or_null, 'r : value_or_null) result =
      | Value : ('a : value_or_null). ('a, 'a) result
      | Or_canceled : ('a : value_or_null). ('a, 'a Or_canceled.t) result

    let[@inline] force_as
      (type (a : value_or_null) (r : value_or_null))
      cap
      c
      (t : a t)
      (r : (a, r) result)
      : r
      =
      let w = C.unsafe_to_await cap in
      (* Note that there are no backoffs in this function because the state machine has no
         loops - every time we recurse we know we're going to be getting the contents of
         an already-forced lazy value *)
      let[@inline] rec loop () : r =
        let prev = Awaitable.get t in
        let[@inline] wait ~until_phys_unequal_to : r =
          match r with
          | Value ->
            (match Awaitable.await w t ~until_phys_unequal_to with
             | Signaled -> loop ()
             | Terminated -> raise Await.Terminated)
          | Or_canceled ->
            (match Awaitable.await_or_cancel w c t ~until_phys_unequal_to with
             | Signaled -> loop ()
             | Canceled -> Canceled
             | Terminated -> raise Await.Terminated)
        in
        match prev with
        | Computed value ->
          (match r with
           | Value -> value
           | Or_canceled -> Completed value)
        | Error exn -> raise exn
        | Computing ->
          (* The lazy value is already being forced, but nobody else is waiting for it.
             Try to change the state to indicate that we want to know when forcing is done *)
          let awaiters = Awaiters in
          (match
             Awaitable.compare_and_set t ~if_phys_equal_to:prev ~replace_with:awaiters
           with
           | Compare_failed ->
             (* The state changed out from under us! Probably this means the force
                finished; we recurse once around which should result in us returning the
                result *)
             loop ()
           | Set_here -> wait ~until_phys_unequal_to:awaiters [@nontail])
        | Awaiters ->
          (* The lazy value is already being forced, and other threads are already waiting
             for it. Join the queue. *)
          wait ~until_phys_unequal_to:prev [@nontail]
        | Uncomputed f ->
          (* The lazy value has not yet been computed. Compute it. *)
          let computing = Computing in
          (match
             Awaitable.compare_and_set t ~if_phys_equal_to:prev ~replace_with:computing
           with
           | Compare_failed -> loop ()
           | Set_here ->
             let computed =
               try Computed (f #(cap, { global = t })) with
               | exn -> Error exn
             in
             (match Awaitable.exchange t computed with
              | Awaiters -> Awaitable.broadcast t
              | Computed _ | Error _ | Computing | Uncomputed _ -> ());
             (match computed with
              | Computed value ->
                (match r with
                 | Value -> value
                 | Or_canceled -> Completed value)
              | Error exn -> raise exn
              | _ ->
                failwith
                  "Invariant: we know we just forced the lazy, so it must be either \
                   computed or error"))
      in
      loop () [@nontail]
    ;;

    let force cap t = force_as cap Cancellation.never t Value
    let force_or_cancel cap c t = force_as cap c t Or_canceled

    let peek t =
      match Awaitable.get t with
      | Computed value -> This value
      | Computing | Awaiters | Uncomputed _ | Error _ -> Null
    ;;

    let peek_opt t =
      match Awaitable.get t with
      | Computed value -> Some value
      | Computing | Awaiters | Uncomputed _ | Error _ -> None
    ;;

    let is_val t =
      match Awaitable.get t with
      | Computed _ -> true
      | Computing | Awaiters | Uncomputed _ | Error _ -> false
    ;;
  end
end

module Sync = Expert.Make (Lock_common.Sync)
module Await = Expert.Make (Lock_common.Await)
