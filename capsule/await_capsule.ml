open! Base
open Import
include Await_capsule_intf
include Capsule

module Sync = struct
  include Definitions (Sync)

  module Mutex = struct
    type 'k t = 'k Sync.Mutex.t

    [@@@ocaml.warning "-incompatible-with-upstream"]

    type packed = P : 'k t -> packed [@@unboxed]

    let create () =
      let (P (type k) (key : k Capsule.Prim.Key.t)) = Capsule.Prim.create () in
      P (Sync.Mutex.create key)
    ;;

    let create_m () : (module Module_with_mutex) =
      let (P (type k) (t : k t)) = create () in
      (module struct
        type nonrec k = k

        let mutex = t
      end)
    ;;

    module Create () = (val create_m ())

    let[@inline] with_lock s t ~f =
      (Sync.Mutex.with_access s t ~f:(fun s access ->
         { global = { aliased_many = f s access } }))
        .global
        .aliased_many
    ;;

    let[@inline] with_lock_or_cancel s c t ~f : _ Await_kernel.Or_canceled.t =
      match
        Sync.Mutex.with_access_or_cancel s c t ~f:(fun s access ->
          { global = { aliased_many = f s access } })
      with
      | Completed { global = { aliased_many = res } } -> Completed res
      | Canceled -> Canceled
    ;;

    module Poisoning = struct
      let[@inline] with_lock s t ~f =
        (Sync.Mutex.with_access_poisoning s t ~f:(fun s access ->
           { global = { aliased_many = f s access } }))
          .global
          .aliased_many
      ;;

      let[@inline] with_lock_or_cancel s c t ~f : _ Await_kernel.Or_canceled.t =
        match
          Sync.Mutex.with_access_or_cancel_poisoning s c t ~f:(fun s access ->
            { global = { aliased_many = f s access } })
        with
        | Completed { global = { aliased_many = res } } -> Completed res
        | Canceled -> Canceled
      ;;
    end

    module Expert = Sync.Mutex
  end

  module With_mutex = struct
    type 'a t =
      | P :
          { data : ('a, 'k) Capsule.Data.t
          ; mutex : 'k Mutex.t
          }
          -> 'a t

    let create f =
      let (P mutex) = Mutex.create () in
      let data = Capsule.Data.create f in
      P { data; mutex }
    ;;

    let of_owned owned =
      let (P #{ key; data }) = Capsule.Owned.to_repr owned in
      let mutex = Sync.Mutex.create key in
      P { mutex; data }
    ;;

    let with_lock s (P { mutex; data }) ~f =
      Mutex.with_lock s mutex ~f:(fun s access -> f s (Capsule.Data.unwrap ~access data))
      [@nontail]
    ;;

    let with_lock_or_cancel s c (P { mutex; data }) ~f =
      Mutex.with_lock_or_cancel s c mutex ~f:(fun s access ->
        f s (Capsule.Data.unwrap ~access data))
      [@nontail]
    ;;

    let with_scoped s (P { mutex; data }) ~f =
      (Mutex.Expert.with_password s mutex ~f:(fun sync password ->
         { aliased_many = f sync (Scoped.P #{ data; password }) }))
        .aliased_many
    ;;

    let with_scoped_or_cancel s c (P { mutex; data }) ~f : _ Await_kernel.Or_canceled.t =
      match
        Mutex.Expert.with_password_or_cancel s c mutex ~f:(fun sync password ->
          { aliased_many = f sync (Scoped.P #{ data; password }) })
      with
      | Completed { aliased_many = res } -> Completed res
      | Canceled -> Canceled
    ;;

    module Poisoning = struct
      let with_lock s (P { mutex; data }) ~f =
        Mutex.Poisoning.with_lock s mutex ~f:(fun s access ->
          f s (Capsule.Data.unwrap ~access data))
        [@nontail]
      ;;

      let with_lock_or_cancel s c (P { mutex; data }) ~f =
        Mutex.Poisoning.with_lock_or_cancel s c mutex ~f:(fun s access ->
          f s (Capsule.Data.unwrap ~access data))
        [@nontail]
      ;;
    end

    let iter = with_lock

    let map s (P { mutex; data }) ~f =
      let data =
        (Mutex.with_lock s mutex ~f:(fun s access ->
           { aliased = Capsule.Data.wrap ~access (f s (Capsule.Data.unwrap ~access data))
           }))
          .aliased
      in
      P { mutex; data }
    ;;

    let destroy s (P { mutex; data }) =
      let key = Sync.Mutex.acquire_and_poison s mutex in
      let access = Capsule.Prim.Key.destroy key in
      Capsule.Prim.Data.unwrap data ~access
    ;;
  end

  module Rwlock = struct
    type 'k t = 'k Sync.Rwlock.t

    [@@@ocaml.warning "-incompatible-with-upstream"]

    type packed = P : 'k t -> packed [@@unboxed]

    let create () =
      let (P (type k) (key : k Capsule.Prim.Key.t)) = Capsule.Prim.create () in
      P (Sync.Rwlock.create key)
    ;;

    let create_m () : (module Module_with_rwlock) =
      let (P (type k) (t : k t)) = create () in
      (module struct
        type nonrec k = k

        let rwlock = t
      end)
    ;;

    module Create () = (val create_m ())

    let[@inline] with_write s t ~f =
      (Sync.Rwlock.with_access s t ~f:(fun s access ->
         { global = { aliased = { many = f s access } } }))
        .global
        .aliased
        .many
    ;;

    let[@inline] with_write_or_cancel s c t ~f : _ Await_kernel.Or_canceled.t =
      match
        Sync.Rwlock.with_access_or_cancel s c t ~f:(fun s access ->
          { global = { aliased = { many = f s access } } })
      with
      | Completed { global = { aliased = { many = res } } } -> Completed res
      | Canceled -> Canceled
    ;;

    let[@inline] with_read s t ~f =
      (Sync.Rwlock.with_access_shared s t ~f:(fun s access ->
         { global = { aliased = { many = f s access } } }))
        .global
        .aliased
        .many
    ;;

    let[@inline] with_read_or_cancel s c t ~f : _ Await_kernel.Or_canceled.t =
      match
        Sync.Rwlock.with_access_shared_or_cancel s c t ~f:(fun s access ->
          { global = { aliased = { many = f s access } } })
      with
      | Completed { global = { aliased = { many = res } } } -> Completed res
      | Canceled -> Canceled
    ;;

    module Poisoning = struct
      let[@inline] with_write s t ~f =
        (Sync.Rwlock.with_access_poisoning s t ~f:(fun s access ->
           { global = { aliased = { many = f s access } } }))
          .global
          .aliased
          .many
      ;;

      let[@inline] with_write_or_cancel s c t ~f : _ Await_kernel.Or_canceled.t =
        match
          Sync.Rwlock.with_access_or_cancel_poisoning s c t ~f:(fun s access ->
            { global = { aliased = { many = f s access } } })
        with
        | Completed { global = { aliased = { many = res } } } -> Completed res
        | Canceled -> Canceled
      ;;

      let[@inline] with_read s t ~f =
        (Sync.Rwlock.with_access_shared_freezing s t ~f:(fun s access ->
           { global = { aliased = { many = f s access } } }))
          .global
          .aliased
          .many
      ;;

      let[@inline] with_read_or_cancel s c t ~f : _ Await_kernel.Or_canceled.t =
        match
          Sync.Rwlock.with_access_shared_or_cancel_freezing s c t ~f:(fun s access ->
            { global = { aliased = { many = f s access } } })
        with
        | Completed { global = { aliased = { many = res } } } -> Completed res
        | Canceled -> Canceled
      ;;
    end

    module Expert = Sync.Rwlock
  end

  module With_rwlock = struct
    type 'a t =
      | P :
          { data : ('a, 'k) Capsule.Data.t
          ; rwlock : 'k Rwlock.t
          }
          -> 'a t

    let create f =
      let (P rwlock) = Rwlock.create () in
      let data = Capsule.Data.create f in
      P { data; rwlock }
    ;;

    let of_owned owned =
      let (P #{ key; data }) = Capsule.Owned.to_repr owned in
      let rwlock = Sync.Rwlock.create key in
      P { rwlock; data }
    ;;

    let with_write s (P { rwlock; data }) ~f =
      Rwlock.with_write s rwlock ~f:(fun s access ->
        f s (Capsule.Data.unwrap ~access data))
      [@nontail]
    ;;

    let with_write_or_cancel s c (P { rwlock; data }) ~f =
      Rwlock.with_write_or_cancel s c rwlock ~f:(fun s access ->
        f s (Capsule.Data.unwrap ~access data))
      [@nontail]
    ;;

    let with_scoped s (P { rwlock; data }) ~f =
      (Rwlock.Expert.with_password s rwlock ~f:(fun sync password ->
         { aliased_many = f sync (Scoped.P #{ password; data }) }))
        .aliased_many
    ;;

    let with_scoped_or_cancel s c (P { rwlock; data }) ~f : _ Await_kernel.Or_canceled.t =
      match
        Rwlock.Expert.with_password_or_cancel s c rwlock ~f:(fun sync password ->
          { aliased_many = f sync (Scoped.P #{ password; data }) })
      with
      | Completed { aliased_many = res } -> Completed res
      | Canceled -> Canceled
    ;;

    let with_read s (P { rwlock; data }) ~f =
      Rwlock.with_read s rwlock ~f:(fun s access ->
        f s ((Capsule.Data.unwrap [@mode shared]) ~access data))
      [@nontail]
    ;;

    let with_read_or_cancel s c (P { rwlock; data }) ~f =
      Rwlock.with_read_or_cancel s c rwlock ~f:(fun s access ->
        f s ((Capsule.Data.unwrap [@mode shared]) ~access data))
      [@nontail]
    ;;

    (* SAFETY:

       1. [shared] is [[@@unboxed]], meaning ['a] and
          ['a shared have the same representation]
       2. the modality transformation is sound for all monadic modalities
    *)
    external wrap_shared
      :  ('a, 'k) Capsule.Data.t
      -> ('a shared, 'k) Capsule.Data.t
      @@ stateless
      = "%identity"

    let with_scoped_shared s (P { rwlock; data }) ~f =
      (Rwlock.Expert.with_password_shared s rwlock ~f:(fun sync password ->
         { aliased_many = f sync (Scoped.Shared.P #{ password; data = wrap_shared data })
         }))
        .aliased_many
    ;;

    let with_scoped_shared_or_cancel s c (P { rwlock; data }) ~f
      : _ Await_kernel.Or_canceled.t
      =
      match
        Rwlock.Expert.with_password_shared_or_cancel s c rwlock ~f:(fun sync password ->
          { aliased_many = f sync (Scoped.Shared.P #{ password; data = wrap_shared data })
          })
      with
      | Completed { aliased_many = res } -> Completed res
      | Canceled -> Canceled
    ;;

    module Poisoning = struct
      let with_write s (P { rwlock; data }) ~f =
        Rwlock.Poisoning.with_write s rwlock ~f:(fun s access ->
          f s (Capsule.Data.unwrap ~access data))
        [@nontail]
      ;;

      let with_write_or_cancel s c (P { rwlock; data }) ~f =
        Rwlock.Poisoning.with_write_or_cancel s c rwlock ~f:(fun s access ->
          f s (Capsule.Data.unwrap ~access data))
        [@nontail]
      ;;

      let with_scoped s (P { rwlock; data }) ~f =
        (Rwlock.Expert.with_password_poisoning s rwlock ~f:(fun sync password ->
           { aliased_many = f sync (Scoped.P #{ password; data }) }))
          .aliased_many
      ;;

      let with_scoped_or_cancel s c (P { rwlock; data }) ~f : _ Await_kernel.Or_canceled.t
        =
        match
          Rwlock.Expert.with_password_or_cancel_poisoning
            s
            c
            rwlock
            ~f:(fun sync password ->
              { aliased_many = f sync (Scoped.P #{ password; data }) })
        with
        | Completed { aliased_many = res } -> Completed res
        | Canceled -> Canceled
      ;;

      let with_read s (P { rwlock; data }) ~f =
        Rwlock.Poisoning.with_read s rwlock ~f:(fun s access ->
          f s ((Capsule.Data.unwrap [@mode shared]) ~access data))
        [@nontail]
      ;;

      let with_read_or_cancel s c (P { rwlock; data }) ~f =
        Rwlock.Poisoning.with_read_or_cancel s c rwlock ~f:(fun s access ->
          f s ((Capsule.Data.unwrap [@mode shared]) ~access data))
        [@nontail]
      ;;

      let with_scoped_shared s (P { rwlock; data }) ~f =
        (Rwlock.Expert.with_password_shared s rwlock ~f:(fun sync password ->
           { aliased_many =
               f sync (Scoped.Shared.P #{ password; data = wrap_shared data })
           }))
          .aliased_many
      ;;

      let with_scoped_shared_or_cancel s c (P { rwlock; data }) ~f
        : _ Await_kernel.Or_canceled.t
        =
        match
          Rwlock.Expert.with_password_shared_or_cancel_freezing
            s
            c
            rwlock
            ~f:(fun sync password ->
              { aliased_many =
                  f sync (Scoped.Shared.P #{ password; data = wrap_shared data })
              })
        with
        | Completed { aliased_many = res } -> Completed res
        | Canceled -> Canceled
      ;;
    end

    let iter_write = with_write
    let iter_read = with_read
  end
end

module Await = struct
  include Definitions (Await)

  module Mutex = struct
    type 'k t = 'k Await.Mutex.t

    [@@@ocaml.warning "-incompatible-with-upstream"]

    type packed = P : 'k t -> packed [@@unboxed]

    let create () =
      let (P (type k) (key : k Capsule.Prim.Key.t)) = Capsule.Prim.create () in
      P (Await.Mutex.create key)
    ;;

    let create_m () : (module Module_with_mutex) =
      let (P (type k) (t : k t)) = create () in
      (module struct
        type nonrec k = k

        let mutex = t
      end)
    ;;

    module Create () = (val create_m ())

    let[@inline] with_lock await t ~f =
      (Await.Mutex.with_access await t ~f:(fun access ->
         { global = { aliased_many = f access } }))
        .global
        .aliased_many
    ;;

    let[@inline] with_lock_or_cancel await c t ~f : _ Await_kernel.Or_canceled.t =
      match
        Await.Mutex.with_access_or_cancel await c t ~f:(fun access ->
          { global = { aliased_many = f access } })
      with
      | Completed { global = { aliased_many = res } } -> Completed res
      | Canceled -> Canceled
    ;;

    module Poisoning = struct
      let[@inline] with_lock await t ~f =
        (Await.Mutex.with_access_poisoning await t ~f:(fun access ->
           { global = { aliased_many = f access } }))
          .global
          .aliased_many
      ;;

      let[@inline] with_lock_or_cancel await c t ~f : _ Await_kernel.Or_canceled.t =
        match
          Await.Mutex.with_access_or_cancel_poisoning await c t ~f:(fun access ->
            { global = { aliased_many = f access } })
        with
        | Completed { global = { aliased_many = res } } -> Completed res
        | Canceled -> Canceled
      ;;
    end

    module Expert = Await.Mutex
  end

  module With_mutex = struct
    type 'a t =
      | P :
          { data : ('a, 'k) Capsule.Data.t
          ; mutex : 'k Mutex.t
          }
          -> 'a t

    let create f =
      let (P mutex) = Mutex.create () in
      let data = Capsule.Data.create f in
      P { data; mutex }
    ;;

    let of_owned owned =
      let (P #{ key; data }) = Capsule.Owned.to_repr owned in
      let mutex = Await.Mutex.create key in
      P { mutex; data }
    ;;

    let with_lock await (P { mutex; data }) ~f =
      Mutex.with_lock await mutex ~f:(fun access -> f (Capsule.Data.unwrap ~access data))
      [@nontail]
    ;;

    let with_lock_or_cancel await c (P { mutex; data }) ~f =
      Mutex.with_lock_or_cancel await c mutex ~f:(fun access ->
        f (Capsule.Data.unwrap ~access data))
      [@nontail]
    ;;

    let with_scoped await (P { mutex; data }) ~f =
      (Mutex.Expert.with_password await mutex ~f:(fun password ->
         { aliased_many = f (Scoped.P #{ data; password }) }))
        .aliased_many
    ;;

    let with_scoped_or_cancel await c (P { mutex; data }) ~f
      : _ Await_kernel.Or_canceled.t
      =
      match
        Mutex.Expert.with_password_or_cancel await c mutex ~f:(fun password ->
          { aliased_many = f (Scoped.P #{ data; password }) })
      with
      | Completed { aliased_many = res } -> Completed res
      | Canceled -> Canceled
    ;;

    module Poisoning = struct
      let with_lock await (P { mutex; data }) ~f =
        Mutex.Poisoning.with_lock await mutex ~f:(fun access ->
          f (Capsule.Data.unwrap ~access data))
        [@nontail]
      ;;

      let with_lock_or_cancel await c (P { mutex; data }) ~f =
        match
          Mutex.Poisoning.with_lock_or_cancel await c mutex ~f:(fun access ->
            f (Capsule.Data.unwrap ~access data))
        with
        | Completed res -> Await_kernel.Or_canceled.Completed res
        | Canceled -> Canceled
      ;;
    end

    let iter = with_lock

    let map await (P { mutex; data }) ~f =
      let data =
        (Mutex.with_lock await mutex ~f:(fun access ->
           { aliased = Capsule.Data.wrap ~access (f (Capsule.Data.unwrap ~access data)) }))
          .aliased
      in
      P { mutex; data }
    ;;

    let destroy await (P { mutex; data }) =
      let key = Await.Mutex.acquire_and_poison await mutex in
      let access = Capsule.Prim.Key.destroy key in
      Capsule.Prim.Data.unwrap data ~access
    ;;
  end

  module Rwlock = struct
    type 'k t = 'k Await.Rwlock.t

    [@@@ocaml.warning "-incompatible-with-upstream"]

    type packed = P : 'k t -> packed [@@unboxed]

    let create () =
      let (P (type k) (key : k Capsule.Prim.Key.t)) = Capsule.Prim.create () in
      P (Await.Rwlock.create key)
    ;;

    let create_m () : (module Module_with_rwlock) =
      let (P (type k) (t : k t)) = create () in
      (module struct
        type nonrec k = k

        let rwlock = t
      end)
    ;;

    module Create () = (val create_m ())

    let[@inline] with_write await t ~f =
      (Await.Rwlock.with_access await t ~f:(fun access ->
         { global = { aliased = { many = f access } } }))
        .global
        .aliased
        .many
    ;;

    let[@inline] with_read await t ~f =
      (Await.Rwlock.with_access_shared await t ~f:(fun access ->
         { global = { aliased = { many = f access } } }))
        .global
        .aliased
        .many
    ;;

    let[@inline] with_write_or_cancel await c t ~f : _ Await_kernel.Or_canceled.t =
      match
        Await.Rwlock.with_access_or_cancel await c t ~f:(fun access ->
          { global = { aliased = { many = f access } } })
      with
      | Completed { global = { aliased = { many = res } } } -> Completed res
      | Canceled -> Canceled
    ;;

    let[@inline] with_read_or_cancel await c t ~f : _ Await_kernel.Or_canceled.t =
      match
        Await.Rwlock.with_access_shared_or_cancel await c t ~f:(fun access ->
          { global = { aliased = { many = f access } } })
      with
      | Completed { global = { aliased = { many = res } } } -> Completed res
      | Canceled -> Canceled
    ;;

    module Poisoning = struct
      let[@inline] with_write await t ~f =
        (Await.Rwlock.with_access_poisoning await t ~f:(fun access ->
           { global = { aliased = { many = f access } } }))
          .global
          .aliased
          .many
      ;;

      let[@inline] with_write_or_cancel await c t ~f : _ Await_kernel.Or_canceled.t =
        match
          Await.Rwlock.with_access_or_cancel_poisoning await c t ~f:(fun access ->
            { global = { aliased = { many = f access } } })
        with
        | Completed { global = { aliased = { many = res } } } -> Completed res
        | Canceled -> Canceled
      ;;

      let[@inline] with_read await t ~f =
        (Await.Rwlock.with_access_shared_freezing await t ~f:(fun access ->
           { global = { aliased = { many = f access } } }))
          .global
          .aliased
          .many
      ;;

      let[@inline] with_read_or_cancel await c t ~f : _ Await_kernel.Or_canceled.t =
        match
          Await.Rwlock.with_access_shared_or_cancel_freezing await c t ~f:(fun access ->
            { global = { aliased = { many = f access } } })
        with
        | Completed { global = { aliased = { many = res } } } -> Completed res
        | Canceled -> Canceled
      ;;
    end

    module Expert = Await.Rwlock
  end

  module With_rwlock = struct
    type 'a t =
      | P :
          { data : ('a, 'k) Capsule.Data.t
          ; rwlock : 'k Rwlock.t
          }
          -> 'a t

    let create f =
      let (P rwlock) = Rwlock.create () in
      let data = Capsule.Data.create f in
      P { data; rwlock }
    ;;

    let of_owned owned =
      let (P #{ key; data }) = Capsule.Owned.to_repr owned in
      let rwlock = Await.Rwlock.create key in
      P { rwlock; data }
    ;;

    let with_write await (P { rwlock; data }) ~f =
      Rwlock.with_write await rwlock ~f:(fun access ->
        f (Capsule.Data.unwrap ~access data))
      [@nontail]
    ;;

    let with_write_or_cancel await c (P { rwlock; data }) ~f =
      Rwlock.with_write_or_cancel await c rwlock ~f:(fun access ->
        f (Capsule.Data.unwrap ~access data))
      [@nontail]
    ;;

    let with_scoped await (P { rwlock; data }) ~f =
      (Rwlock.Expert.with_password await rwlock ~f:(fun password ->
         { aliased_many = f (Scoped.P #{ password; data }) }))
        .aliased_many
    ;;

    let with_scoped_or_cancel await c (P { rwlock; data }) ~f
      : _ Await_kernel.Or_canceled.t
      =
      match
        Rwlock.Expert.with_password_or_cancel await c rwlock ~f:(fun password ->
          { aliased_many = f (Scoped.P #{ password; data }) })
      with
      | Completed { aliased_many = res } -> Completed res
      | Canceled -> Canceled
    ;;

    let with_read await (P { rwlock; data }) ~f =
      Rwlock.with_read await rwlock ~f:(fun access ->
        f ((Capsule.Data.unwrap [@mode shared]) ~access data))
      [@nontail]
    ;;

    let with_read_or_cancel await c (P { rwlock; data }) ~f =
      Rwlock.with_read_or_cancel await c rwlock ~f:(fun access ->
        f ((Capsule.Data.unwrap [@mode shared]) ~access data))
      [@nontail]
    ;;

    (* SAFETY:

       1. [shared] is [[@@unboxed]], meaning ['a] and
          ['a shared have the same representation]
       2. the modality transformation is sound for all monadic modalities
    *)
    external wrap_shared
      :  ('a, 'k) Capsule.Data.t
      -> ('a shared, 'k) Capsule.Data.t
      @@ stateless
      = "%identity"

    let with_scoped_shared await (P { rwlock; data }) ~f =
      (Rwlock.Expert.with_password_shared await rwlock ~f:(fun password ->
         { aliased_many = f (Scoped.Shared.P #{ password; data = wrap_shared data }) }))
        .aliased_many
    ;;

    let with_scoped_shared_or_cancel await c (P { rwlock; data }) ~f
      : _ Await_kernel.Or_canceled.t
      =
      match
        Rwlock.Expert.with_password_shared_or_cancel await c rwlock ~f:(fun password ->
          { aliased_many = f (Scoped.Shared.P #{ password; data = wrap_shared data }) })
      with
      | Completed { aliased_many = res } -> Completed res
      | Canceled -> Canceled
    ;;

    module Poisoning = struct
      let with_write await (P { rwlock; data }) ~f =
        Rwlock.Poisoning.with_write await rwlock ~f:(fun access ->
          f (Capsule.Data.unwrap ~access data))
        [@nontail]
      ;;

      let with_write_or_cancel await c (P { rwlock; data }) ~f =
        Rwlock.Poisoning.with_write_or_cancel await c rwlock ~f:(fun access ->
          f (Capsule.Data.unwrap ~access data))
        [@nontail]
      ;;

      let with_scoped await (P { rwlock; data }) ~f =
        (Rwlock.Expert.with_password_poisoning await rwlock ~f:(fun password ->
           { aliased_many = f (Scoped.P #{ password; data }) }))
          .aliased_many
      ;;

      let with_scoped_or_cancel await c (P { rwlock; data }) ~f
        : _ Await_kernel.Or_canceled.t
        =
        match
          Rwlock.Expert.with_password_or_cancel_poisoning
            await
            c
            rwlock
            ~f:(fun password -> { aliased_many = f (Scoped.P #{ password; data }) })
        with
        | Completed { aliased_many = res } -> Completed res
        | Canceled -> Canceled
      ;;

      let with_read await (P { rwlock; data }) ~f =
        Rwlock.Poisoning.with_read await rwlock ~f:(fun access ->
          f ((Capsule.Data.unwrap [@mode shared]) ~access data))
        [@nontail]
      ;;

      let with_read_or_cancel await c (P { rwlock; data }) ~f =
        Rwlock.Poisoning.with_read_or_cancel await c rwlock ~f:(fun access ->
          f ((Capsule.Data.unwrap [@mode shared]) ~access data))
        [@nontail]
      ;;

      let with_scoped_shared await (P { rwlock; data }) ~f =
        (Rwlock.Expert.with_password_shared_freezing await rwlock ~f:(fun password ->
           { aliased_many = f (Scoped.Shared.P #{ password; data = wrap_shared data }) }))
          .aliased_many
      ;;

      let with_scoped_shared_or_cancel await c (P { rwlock; data }) ~f
        : _ Await_kernel.Or_canceled.t
        =
        match
          Rwlock.Expert.with_password_shared_or_cancel_freezing
            await
            c
            rwlock
            ~f:(fun password ->
              { aliased_many = f (Scoped.Shared.P #{ password; data = wrap_shared data })
              })
        with
        | Completed { aliased_many = res } -> Completed res
        | Canceled -> Canceled
      ;;
    end

    let iter_write = with_write
    let iter_read = with_read
  end
end
