open! Base
open Portable_kernel
open Await_sync
include Await_capsule_intf.Definitions
include Capsule

module Mutex = struct
  type 'k t = 'k Mutex.t
  type packed = P : 'k t -> packed

  let create () =
    let (P (type k) (key : k Capsule.Expert.Key.t)) = Capsule.Expert.create () in
    P (Mutex.create key)
  ;;

  let create_m () : (module Module_with_mutex) =
    let (P (type k) (t : k t)) = create () in
    (module struct
      type nonrec k = k

      let mutex = t
    end)
  ;;

  module Create () = (val create_m ())

  let[@inline] with_lock (await @ local) t ~f =
    (Mutex.with_access await t ~f:(fun access ->
       { global = { aliased = { many = f access } } }))
      .global
      .aliased
      .many
  ;;
end

module With_mutex = struct
  type ('a, 'k) inner : value mod contended portable =
    { data : ('a, 'k) Capsule.Data.t
    ; mutex : 'k Mutex.t
    }

  type 'a t : value mod contended portable = P : ('a, 'k) inner -> 'a t [@@unboxed]

  let create f =
    let (P mutex) = Mutex.create () in
    let data = Capsule.Data.create f in
    P { data; mutex }
  ;;

  let of_isolated (Capsule.Isolated.P { key; data }) =
    let mutex = Await_sync.Mutex.create key in
    P { mutex; data }
  ;;

  let with_lock await (P { mutex; data }) ~f =
    Mutex.with_lock await mutex ~f:(fun access -> f (Capsule.Data.unwrap ~access data))
    [@nontail]
  ;;

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
    let key = Await_sync.Mutex.acquire_and_poison await mutex in
    let access = Capsule.Expert.Key.destroy key in
    Capsule.Expert.Data.unwrap data ~access
  ;;
end
