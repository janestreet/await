open Base
open Await_sync

module Make (T : sig
    type t

    val create : unit -> t @@ stateless
  end) =
struct
  type 'k inner =
    #{ mutex : 'k Sync.Mutex.t
     ; data : (T.t or_null ref, 'k) Capsule.Data.t
     }

  type pack = P : 'k inner -> pack [@@unboxed]
  type t = pack iarray

  let create_one () =
    let (P key) = Capsule.Prim.create () in
    let mutex = Sync.Mutex.create key in
    let data = Capsule.Data.create (fun () -> ref Null) in
    P #{ mutex; data }
  ;;

  let create ?(shards = 1) () =
    let shards = 1 lsl Int.ceil_log2 shards in
    let t = Array.create ~len:shards (create_one ()) in
    for i = 1 to shards - 1 do
      t.(i) <- create_one ()
    done;
    Iarray.unsafe_of_array__promise_no_mutation t
  ;;

  let access t ~f =
    let (P #{ mutex; data }) =
      (* Length is always a power of two. *)
      Iarray.unsafe_get t (Domain.self_index () land (Iarray.length t - 1))
    in
    match
      Sync.Mutex.try_with_access mutex ~f:(fun [@inline] access ->
        let state = Capsule.Prim.Data.Local.unwrap ~access data in
        let state =
          match !state with
          | Null ->
            let fresh = T.create () in
            state := This fresh;
            fresh
          | This state -> state
        in
        { aliased_many = f state })
    with
    | Would_block -> f (T.create ())
    | Acquired { aliased_many } -> aliased_many
  ;;

  let iter = access
end
