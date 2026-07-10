open Base
open Import
module Capsule = Capsule.Prim

module
  [@inline] Make
    (Prim : Mutex_intf.Prim)
    (C : Lock_common_intf.Capability_with_of_await) =
struct
  type 'k t = Prim.t

  let create ?padded _key = Prim.create ?padded ()

  module Guard = struct
    type 'k mutex = 'k t

    type 'k inner =
      { mutex : 'k mutex @@ many
      ; mutable should_poison : bool [@atomic]
      }

    type 'k t = { inner : 'k inner @@ aliased contended portable } [@@unboxed]

    let poison_if_locked ({ mutex; _ } as inner) =
      if Atomic.Loc.get [%atomic.loc inner.should_poison] then Prim.poison mutex
    ;;

    let create : 'k mutex -> 'k t @ unique =
      fun mutex ->
      let t = { mutex; should_poison = true } in
      Stdlib.Gc.Safe.finalise poison_if_locked t;
      { inner = t }
    ;;

    let with_key
      :  'k t @ unique
      -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ once unique)
         @ local once
      -> 'a * 'k t @ once unique
      =
      fun t ~f ->
      match f (Capsule.Key.unsafe_mk ()) with
      | #(res, _key) -> res, t
      | exception exn -> Prim.poison_and_reraise exn t.inner.mutex
    ;;

    let with_password
      :  'k t @ unique -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
      -> 'a * 'k t @ unique
      =
      fun t ~f ->
      let { many = a }, t =
        with_key
          t
          ~f:
            (Capsule.Key.with_password ~f:(fun password -> { many = f password })
            [@nontail])
      in
      a, t
    ;;

    let access
      :  'k t @ unique
      -> f:('k Capsule.Access.t -> 'a @ contended once portable unique)
         @ local once portable
      -> 'a * 'k t @ contended once portable unique
      =
      fun t ~f ->
      let { contended = { portable = a } }, t =
        with_key t ~f:(fun key ->
          let #(a, key) =
            Capsule.Key.access key ~f:(fun access -> { portable = f access })
          in
          #({ contended = a }, key))
      in
      a, t
    ;;

    let release { inner = { mutex; _ } as inner } =
      Atomic.Loc.set [%atomic.loc inner.should_poison] false;
      (* Make sure the stack root for [inner] stays alive at least this long, to make sure
         that if its finalizer runs, it sees [should_poison] set to [false]. *)
      let _ : _ = (Sys.opaque_identity [@mode contended]) inner in
      Prim.release mutex
    ;;

    let poison : 'k t @ unique -> 'k Capsule.Key.t @ unique =
      fun { inner = { mutex; _ } as inner } ->
      (* Not technically releasing, but setting this value to [false] prevents calling
         [poison] again in the finalizer, which is moderately more efficient. *)
      Atomic.Loc.set [%atomic.loc inner.should_poison] false;
      Prim.poison mutex;
      Capsule.Key.unsafe_mk ()
    ;;

    let is_poisoning { inner } = Atomic.Loc.get [%atomic.loc inner.should_poison]
  end

  let acquire w t =
    Prim.acquire w t;
    Guard.create t
  ;;

  let try_acquire t : _ Or_would_block.t =
    match Prim.try_acquire t with
    | true -> Acquired (Guard.create t)
    | false -> Would_block
  ;;

  let acquire_or_cancel
    : C.t @ local -> Cancellation.t @ local -> 'k t -> 'k Guard.t Or_canceled.t @ unique
    =
    fun w c t ->
    match Prim.acquire_or_cancel (C.unsafe_to_await w) c t with
    | Canceled -> Canceled
    | Completed () -> Completed (Guard.create t)
  ;;

  module Unsafe = struct
    (* SAFETY:

       Since these functions unlock the mutex on uncaught exceptions rather than
       poisoning, they are unsound: it's possible to stash the provided key in a raised
       exception, which leaks the key. We define them internally to implement functions
       like [with_access] and [with_password], but intentionally don't expose them in the
       interface.
    *)

    [%%template
    [@@@mode.default l = (local, global)]

    let[@inline] with_key
      : type (a : value_or_null) k.
        C.t @ local
        -> k t @ local
        -> f:(k Capsule.Key.t @ unique -> #(a * k Capsule.Key.t) @ l once unique)
           @ local once
        -> a @ l once unique
      =
      fun w t ~f ->
      Prim.acquire (C.unsafe_to_await w) t;
      match[@exclave_if_local l ~reasons:[ May_return_local ]]
        f (Capsule.Key.unsafe_mk ())
      with
      | #(res, _key) ->
        Prim.release t;
        res
      | exception exn -> Prim.release_and_reraise exn t
    ;;

    let[@inline] try_with_key
      : type (a : value_or_null) k.
        k t @ local
        -> f:(k Capsule.Key.t @ unique -> #(a * k Capsule.Key.t) @ l once unique)
           @ local once
        -> a Or_would_block.t @ l once unique
      =
      fun t ~f ->
      if Prim.try_acquire t
      then (
        match[@exclave_if_local l ~reasons:[ May_return_local ]]
          f (Capsule.Key.unsafe_mk ())
        with
        | #(res, _key) ->
          Prim.release t;
          Acquired res
        | exception exn -> Prim.release_and_reraise exn t)
      else Would_block
    ;;

    let[@inline] with_key_or_cancel
      :  C.t @ local -> Cancellation.t @ local -> 'k t @ local
      -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
         @ local once
      -> 'a Or_canceled.t @ l once unique
      =
      fun w c t ~f ->
      match Prim.acquire_or_cancel (C.unsafe_to_await w) c t with
      | Canceled -> Canceled
      | Completed () ->
        (match[@exclave_if_local l ~reasons:[ May_return_local ]]
           f (Capsule.Key.unsafe_mk ())
         with
         | #(res, _key) ->
           Prim.release t;
           Completed res
         | exception exn -> Prim.release_and_reraise exn t)
    ;;]
  end

  [%%template
  [@@@mode.default l = (local, global)]

  let[@inline] with_key_poisoning
    :  C.t @ local -> 'k t @ local
    -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
       @ local once
    -> 'a @ l once unique
    =
    fun w t ~f ->
    (Prim.acquire (C.unsafe_to_await w) t;
     match f (Capsule.Key.unsafe_mk ()) with
     | #(res, _key) ->
       Prim.release t;
       res
     | exception exn -> Prim.poison_and_reraise exn t)
    [@exclave_if_local l ~reasons:[ May_return_local ]]
  ;;

  let[@inline] try_with_key_poisoning
    :  'k t @ local
    -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
       @ local once
    -> 'a Or_would_block.t @ l once unique
    =
    fun t ~f ->
    (if Prim.try_acquire t
     then (
       match f (Capsule.Key.unsafe_mk ()) with
       | #(res, _key) ->
         Prim.release t;
         Acquired res
       | exception exn -> Prim.poison_and_reraise exn t)
     else Would_block)
    [@exclave_if_local l ~reasons:[ May_return_local ]]
  ;;

  let[@inline] with_key_or_cancel_poisoning
    :  C.t @ local -> Cancellation.t @ local -> 'k t @ local
    -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
       @ local once
    -> 'a Or_canceled.t @ l once unique
    =
    fun w c t ~f ->
    match[@exclave_if_local l ~reasons:[ May_return_local ]]
      Prim.acquire_or_cancel (C.unsafe_to_await w) c t
    with
    | Canceled -> Canceled
    | Completed () ->
      (match f (Capsule.Key.unsafe_mk ()) with
       | #(res, _key) ->
         Prim.release t;
         Completed res
       | exception exn -> Prim.poison_and_reraise exn t)
  ;;

  let with_access_poisoning w t ~f =
    ((with_key_poisoning [@mode l]) w t ~f:(fun key ->
       (let #(a, key) =
          (Capsule.Key.access [@mode l]) key ~f:(fun access ->
            { portable = f access } [@exclave_if_local l ~reasons:[ May_return_local ]])
        in
        #({ contended = a }, key))
       [@exclave_if_local l ~reasons:[ May_return_local ]]))
      .contended
      .portable
      [@exclave_if_local l ~reasons:[ May_return_local ]]
  ;;

  let try_with_access_poisoning t ~f : _ Or_would_block.t @ l =
    match[@exclave_if_local l ~reasons:[ May_return_local ]]
      (try_with_key_poisoning [@mode l]) t ~f:(fun key ->
        (let #(a, key) =
           (Capsule.Key.access [@mode l]) key ~f:(fun access ->
             { portable = f access } [@exclave_if_local l ~reasons:[ May_return_local ]])
         in
         #({ contended = a }, key))
        [@exclave_if_local l ~reasons:[ May_return_local ]])
    with
    | Would_block -> Would_block
    | Acquired { contended = { portable = a } } -> Acquired a
  ;;

  let with_access w t ~f =
    ((Unsafe.with_key [@mode l]) w t ~f:(fun key ->
       (let #(a, key) =
          (Capsule.Key.access [@mode l]) key ~f:(fun access ->
            { portable = f access } [@exclave_if_local l ~reasons:[ May_return_local ]])
        in
        #({ contended = a }, key))
       [@exclave_if_local l ~reasons:[ May_return_local ]]))
      .contended
      .portable
      [@exclave_if_local l ~reasons:[ May_return_local ]]
  ;;

  let try_with_access t ~f : _ Or_would_block.t @ l =
    match[@exclave_if_local l ~reasons:[ May_return_local ]]
      (Unsafe.try_with_key [@mode l]) t ~f:(fun key ->
        (let #(a, key) =
           (Capsule.Key.access [@mode l]) key ~f:(fun access ->
             { portable = f access } [@exclave_if_local l ~reasons:[ May_return_local ]])
         in
         #({ contended = a }, key))
        [@exclave_if_local l ~reasons:[ May_return_local ]])
    with
    | Would_block -> Would_block
    | Acquired { contended = { portable = a } } -> Acquired a
  ;;

  let with_access_or_cancel_poisoning w c t ~f : _ Or_canceled.t =
    match[@exclave_if_local l ~reasons:[ May_return_local ]]
      (with_key_or_cancel_poisoning [@mode l]) w c t ~f:(fun key ->
        (let #(a, key) =
           (Capsule.Key.access [@mode l]) key ~f:(fun access ->
             { portable = f access } [@exclave_if_local l ~reasons:[ May_return_local ]])
         in
         #({ contended = a }, key))
        [@exclave_if_local l ~reasons:[ May_return_local ]])
    with
    | Canceled -> Canceled
    | Completed { contended = { portable = a } } -> Completed a
  ;;

  let with_access_or_cancel w c t ~f : _ Or_canceled.t =
    match[@exclave_if_local l ~reasons:[ May_return_local ]]
      (Unsafe.with_key_or_cancel [@mode l]) w c t ~f:(fun key ->
        (let #(a, key) =
           (Capsule.Key.access [@mode l]) key ~f:(fun access ->
             { portable = f access } [@exclave_if_local l ~reasons:[ May_return_local ]])
         in
         #({ contended = a }, key))
        [@exclave_if_local l ~reasons:[ May_return_local ]])
    with
    | Canceled -> Canceled
    | Completed { contended = { portable = a } } -> Completed a
  ;;]

  let with_password_poisoning w t ~f =
    (with_key_poisoning w t ~f:(fun key ->
       Capsule.Key.with_password key ~f:(fun password -> { many = f password }) [@nontail]))
      .many
  ;;

  let try_with_password_poisoning t ~f : _ Or_would_block.t =
    match
      try_with_key_poisoning t ~f:(fun key ->
        Capsule.Key.with_password key ~f:(fun password -> { many = f password })
        [@nontail])
    with
    | Would_block -> Would_block
    | Acquired { many = a } -> Acquired a
  ;;

  let with_password w t ~f =
    (Unsafe.with_key w t ~f:(fun key ->
       Capsule.Key.with_password key ~f:(fun password -> { many = f password }) [@nontail]))
      .many
  ;;

  let try_with_password t ~f : _ Or_would_block.t =
    match
      Unsafe.try_with_key t ~f:(fun key ->
        Capsule.Key.with_password key ~f:(fun password -> { many = f password })
        [@nontail])
    with
    | Would_block -> Would_block
    | Acquired { many = a } -> Acquired a
  ;;

  let with_password_or_cancel_poisoning w c t ~f : _ Or_canceled.t =
    match
      with_key_or_cancel_poisoning w c t ~f:(fun key ->
        Capsule.Key.with_password key ~f:(fun password -> { many = f password })
        [@nontail])
    with
    | Canceled -> Canceled
    | Completed { many = a } -> Completed a
  ;;

  let with_password_or_cancel w c t ~f : _ Or_canceled.t =
    match
      Unsafe.with_key_or_cancel w c t ~f:(fun key ->
        Capsule.Key.with_password key ~f:(fun password -> { many = f password })
        [@nontail])
    with
    | Canceled -> Canceled
    | Completed { many = a } -> Completed a
  ;;

  let release_temporarily
    :  C.t @ local -> 'k t @ local -> 'k Capsule.Key.t @ unique
    -> f:(unit -> 'a @ unique) @ local once -> #('a * 'k Capsule.Key.t) @ unique
    =
    fun w t k ~f ->
    Prim.release t;
    let res = f () in
    Prim.acquire (C.unsafe_to_await w) t;
    #(res, k)
  ;;

  let release_temporarily_or_cancel
    : ('a : value_or_null).
    C.t @ local
    -> Cancellation.t @ local
    -> 'k t @ local
    -> 'k Capsule.Key.t @ unique
    -> f:(unit -> 'a @ unique) @ local once
    -> (#('a * 'k Capsule.Key.t) Or_canceled.t[@kind value_or_null & void]) @ unique
    =
    fun w c t k ~f ->
    Prim.release t;
    let res = f () in
    match Prim.acquire_or_cancel (C.unsafe_to_await w) c t with
    | Canceled -> Canceled
    | Completed () -> Completed #(res, k)
  ;;

  let acquire_and_poison : C.t @ local -> 'k t @ local -> 'k Capsule.Key.t @ unique =
    fun w t ->
    Prim.acquire (C.unsafe_to_await w) t;
    Prim.poison t;
    Capsule.Key.unsafe_mk ()
  ;;

  let acquire_and_poison_or_cancel
    :  C.t @ local -> Cancellation.t @ local -> 'k t @ local
    -> ('k Capsule.Key.t Or_canceled.t[@kind void]) @ unique
    =
    fun w c t ->
    match Prim.acquire_or_cancel (C.unsafe_to_await w) c t with
    | Canceled -> Canceled
    | Completed () ->
      Prim.poison t;
      Completed (Capsule.Key.unsafe_mk ())
  ;;

  let poison_unacquired : 'k t @ local -> unit = fun t -> Prim.poison t
  let is_poisoned = Prim.is_poisoned
  let is_locked = Prim.is_locked

  let poison t key =
    Prim.poison t;
    key
  ;;

  module Condition = Condition.Make (struct
      type nonrec 'k t = 'k t

      let unsafe_acquire w t = Prim.acquire w t
      let unsafe_release t = Prim.release t
    end)

  [%%template
  [@@@mode.default l = (global, local)]

  let[@inline] with_key_and_condition_wait_poisoning
    : type (a : value_or_null) k.
      Await.t @ local
      -> k t @ local
      -> f:
           (k Condition.Wait.t @ local
            -> k Capsule.Key.t @ unique
            -> #(a * k Capsule.Key.t) @ l once unique)
         @ local once
      -> a @ l once unique
    =
    fun w t ~f ->
    (Prim.acquire w t;
     (Condition.with_wait [@mode l])
       w
       t
       (Capsule.Key.unsafe_mk () : k Capsule.Key.t)
       (fun cw key ->
         match[@exclave_if_local l ~reasons:[ May_return_local ]] f cw key with
         | #(res, _key) ->
           (* If the caller has been able to give a key back to us, the lock must be held *)
           assert%debug (Condition.lock_is_held cw);
           Prim.release t;
           res
         | exception exn ->
           if Condition.lock_is_held cw
           then Prim.poison_and_reraise exn t
           else (
             let bt = Backtrace.Exn.most_recent () in
             Exn.raise_with_original_backtrace exn bt)) [@nontail])
    [@exclave_if_local l ~reasons:[ May_return_local ]]
  ;;

  let with_key_and_condition_wait_or_cancel_poisoning
    : type (a : value_or_null) k.
      Await.t @ local
      -> Cancellation.t @ local
      -> k t @ local
      -> f:
           (k Condition.Wait.t @ local
            -> k Capsule.Key.t @ unique
            -> #(a * k Capsule.Key.t) @ l once unique)
         @ local once
      -> a Or_canceled.t @ l once unique
    =
    fun w c t ~f ->
    match[@exclave_if_local l ~reasons:[ May_return_local ]]
      Prim.acquire_or_cancel w c t
    with
    | Canceled -> Canceled
    | Completed () ->
      (Condition.with_wait [@mode l])
        w
        t
        (Capsule.Key.unsafe_mk () : k Capsule.Key.t)
        (fun cw key ->
          match[@exclave_if_local l ~reasons:[ May_return_local ]] f cw key with
          | #(res, _key) ->
            (* If the caller has been able to give a key back to us, the lock must be held *)
            assert%debug (Condition.lock_is_held cw);
            Prim.release t;
            Or_canceled.Completed res
          | exception exn ->
            if Condition.lock_is_held cw
            then Prim.poison_and_reraise exn t
            else (
              let bt = Backtrace.Exn.most_recent () in
              Exn.raise_with_original_backtrace exn bt)) [@nontail]
  ;;]

  module For_testing = Prim.For_testing
end

module [@inline] Make_sync (Prim : Mutex_intf.Prim) :
  Mutex_intf.Sync with type 'k t = Prim.t = struct
  include Make (Prim) (Lock_common.Sync)

  (* The following definitions shadow the ones defined by Make, but also pass the Sync.t
     to the callback. We can do this here because the actual implementation doesn't
     require an unyielding callback, but the signature exposes these functions as
     requiring unyielding callbacks, which is an important safety property. *)

  [%%template
  [@@@mode.default l = (local, global)]

  let with_access s t ~f =
    (with_access [@mode l]) s t ~f:(fun access ->
      f s access [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]])
    [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]]
  ;;

  let[@inline] with_access_poisoning s t ~f =
    (with_access_poisoning [@mode l]) s t ~f:(fun access ->
      f s access [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]])
    [@exclave_if_local l ~reasons:[ May_return_local ]] [@nontail]
  ;;

  let[@inline] with_access_or_cancel s c t ~f =
    (with_access_or_cancel [@mode l]) s c t ~f:(fun access ->
      f s access [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]])
    [@exclave_if_local l ~reasons:[ May_return_local ]] [@nontail]
  ;;

  let[@inline] with_access_or_cancel_poisoning s c t ~f =
    (with_access_or_cancel_poisoning [@mode l]) s c t ~f:(fun access ->
      f s access [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]])
    [@exclave_if_local l ~reasons:[ May_return_local ]] [@nontail]
  ;;]

  let[@inline] with_password s t ~f =
    with_password s t ~f:(fun password -> f s password [@nontail]) [@nontail]
  ;;

  let[@inline] with_password_poisoning s t ~f =
    with_password_poisoning s t ~f:(fun password -> f s password [@nontail]) [@nontail]
  ;;

  let[@inline] with_password_or_cancel s c t ~f =
    with_password_or_cancel s c t ~f:(fun password -> f s password [@nontail]) [@nontail]
  ;;

  let[@inline] with_password_or_cancel_poisoning s c t ~f =
    with_password_or_cancel_poisoning s c t ~f:(fun password -> f s password [@nontail])
    [@nontail]
  ;;

  [%%template
  [@@@mode.default l = (global, local)]

  let[@inline] with_key_poisoning s t ~f =
    (with_key_poisoning [@mode l]) s t ~f:(fun key ->
      f s key [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]])
    [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]]
  ;;

  let[@inline] with_key_or_cancel_poisoning s c t ~f =
    (with_key_or_cancel_poisoning [@mode l]) s c t ~f:(fun key ->
      f s key [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]])
    [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]]
  ;;

  let[@inline] with_key_and_condition_wait_poisoning w t ~f =
    (with_key_and_condition_wait_poisoning [@mode l]) w t ~f:(fun cw key ->
      f
        (Await.sync w)
        cw
        key [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]])
    [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]]
  ;;

  let[@inline] with_key_and_condition_wait_or_cancel_poisoning w c t ~f =
    (with_key_and_condition_wait_or_cancel_poisoning [@mode l]) w c t ~f:(fun cw key ->
      f
        (Await.sync w)
        cw
        key [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]])
    [@nontail] [@exclave_if_local l ~reasons:[ May_return_local ]]
  ;;]
end

module [@inline] Make_await (Prim : Mutex_intf.Prim) :
  Mutex_intf.Await with type 'k t = Prim.t =
  Make (Prim) (Lock_common.Await)
