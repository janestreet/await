open Base
open Import

module Random_key = struct
  type t = int

  let hash = Fn.id
  let equal = ( = )
end

module Htbl = Portable_lockfree_htbl

let awaiters : (Random_key.t, Trigger.Source.t Nonempty_queue.t) Htbl.t =
  Htbl.create (module Random_key)
;;

let add_new key trigger =
  let data = Nonempty_queue.singleton trigger in
  Or_null.is_null (Htbl.add awaiters ~key ~data)
;;

let find key = Htbl.find awaiters key

let[@inline] set_here_if_phys_equal lhs rhs =
  Bool.select
    (phys_equal lhs rhs)
    Atomic.Compare_failed_or_set_here.Set_here
    Compare_failed
;;

let compare_and_set key ~if_phys_equal_to ~replace_with =
  let prior = Htbl.compare_exchange awaiters key ~if_phys_equal_to ~replace_with in
  set_here_if_phys_equal prior (This if_phys_equal_to)
;;

let compare_remove key ~if_phys_equal_to =
  set_here_if_phys_equal
    (Htbl.compare_remove awaiters key ~if_phys_equal_to)
    (This if_phys_equal_to)
;;

let remove key = Htbl.remove awaiters key

module Drop = struct
  type t =
    | Empty
    | Dropped
    | Not_found
end

module For_testing = struct
  let non_linearizable_length () = Htbl.non_linearizable_length awaiters
end
