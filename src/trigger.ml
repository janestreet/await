open! Base
open! Portable

(* The underlying state machine of a trigger:

     [create]
        |
        v
      Initial---------------[signal]-----------------+
        |                                            |
        |                                            v
        +-[on_signal]-> Awaiting -[signal|drop]-> Signaled
                           ^
                           |
                  [create_with_action]

   The [Signaled] state is terminal. *)
type state =
  | Initial
  | Awaiting of { action : unit -> unit @@ portable }
  | Signaled

type t = state Atomic.t

let is_signalled t =
  match Atomic.get t with
  | Signaled -> true
  | Awaiting _ | Initial -> false
;;

module Source = struct
  type nonrec t = t

  let same = Base.phys_equal

  let signal t =
    match Atomic.get t with
    | Signaled -> ()
    | Awaiting { action } as current ->
      (match
         Atomic.compare_and_set t ~if_phys_equal_to:current ~replace_with:Signaled
       with
       | Set_here -> action ()
       | Compare_failed -> ())
    | Initial as current ->
      (match
         Atomic.compare_exchange t ~if_phys_equal_to:current ~replace_with:Signaled
       with
       | Signaled | Initial -> ()
       | Awaiting { action } as current ->
         (match
            Atomic.compare_and_set t ~if_phys_equal_to:current ~replace_with:Signaled
          with
          | Set_here -> action ()
          | Compare_failed -> ()))
  ;;

  let is_signalled = is_signalled
end

external magic_many : 'a @ once portable -> 'a @ many portable @@ portable = "%identity"

let on_signal t action =
  match Atomic.get t with
  | Signaled -> false
  | Awaiting _ -> failwith "Trigger.on_signal: already awaiting"
  | Initial as if_phys_equal_to ->
    (match
       Atomic.compare_exchange
         t
         ~if_phys_equal_to
         ~replace_with:(Awaiting { action = magic_many action })
     with
     | Initial -> true
     | Signaled -> false
     | Awaiting _ -> failwith "Trigger.on_signal: already awaiting")
;;

let drop t =
  match Atomic.get t with
  | Signaled -> false
  | Initial -> failwith "Trigger.drop: not awaiting"
  | Awaiting _ as if_phys_equal_to ->
    (match Atomic.compare_and_set t ~if_phys_equal_to ~replace_with:Signaled with
     | Set_here -> true
     | Compare_failed -> false)
;;

let%template source t = t [@@mode m = (global, local)]
let create () = Atomic.make Initial
let create_with_action action = Atomic.make (Awaiting { action = magic_many action })
