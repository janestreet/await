open! Base
open! Portable_kernel

(* The underlying state machine of a trigger:

   {v
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
   v}

   The [Signaled] state is terminal. *)
type _ state =
  | Initial : [> `Initial ] state
  | Awaiting :
      { action : 'k @ contended portable unique -> unit @@ portable
      ; k : 'k @@ portable
      }
      -> [> `Awaiting ] state
  | Signaled : [> `Signaled ] state

type t = [ `Initial | `Awaiting | `Signaled ] state Atomic.t

let is_signalled t =
  match Atomic.get t with
  | Signaled -> true
  | Awaiting _ | Initial -> false
;;

module Source = struct
  type nonrec t = t

  let same = Base.phys_equal

  let[@inline never] signal_slow_path t (Awaiting r as current : [ `Awaiting ] state) =
    (* This non-inlined slow path generates two slow calls: the [compare_and_set] involves
       a C call and the [action] calls an unknown function. Keeping this non-inlined
       reduces code size and should not decrease performance significantly. *)
    match Atomic.compare_and_set t ~if_phys_equal_to:current ~replace_with:Signaled with
    | Set_here -> r.action ((Obj.magic_unique [@mode contended portable]) r.k)
    | Compare_failed -> ()
  ;;

  let[@inline] signal t =
    (* The inlined fast path carefully minimizes the size of generated code and makes
       [signal] as fast as possible in the fast cases. *)
    let current = Atomic.get t in
    if not (phys_equal current Signaled)
    then (
      match
        (* This generates an instruction as [Initial] and [Signaled] are both immediates.

           We assume here that the state is [Initial]. In case it isn't, the core will get
           exclusive ownership of the cache line and likely the second [compare_and_set]
           on the [signal_slow_path] will be faster, which means that the extra write is
           likely not as expensive as it might seem. *)
        Atomic.compare_exchange t ~if_phys_equal_to:Initial ~replace_with:Signaled
      with
      | Signaled | Initial -> ()
      | Awaiting _ as current -> signal_slow_path t current)
  ;;

  let is_signalled = is_signalled

  module For_testing = struct
    let signal_if_awaiting t =
      match Atomic.get t with
      | Signaled | Initial -> ()
      | Awaiting current_r as current ->
        (match
           Atomic.compare_and_set t ~if_phys_equal_to:current ~replace_with:Signaled
         with
         | Set_here ->
           current_r.action ((Obj.magic_unique [@mode contended portable]) current_r.k)
         | Compare_failed -> ())
    ;;
  end
end

let on_signal t ~f:action k =
  let k = (Obj.magic_many [@mode contended portable]) k in
  match Atomic.get t with
  | Signaled -> This ((Obj.magic_unique [@mode contended portable]) k)
  | Awaiting _ -> failwith "Trigger.on_signal: already awaiting"
  | Initial as if_phys_equal_to ->
    (match
       Atomic.compare_exchange
         t
         ~if_phys_equal_to
         ~replace_with:(Awaiting { action = (Obj.magic_many [@mode portable]) action; k })
     with
     | Initial -> Null
     | Signaled -> This ((Obj.magic_unique [@mode contended portable]) k)
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

let create_with_action ~f:action k =
  let k = (Obj.magic_many [@mode contended portable]) k in
  Atomic.make (Awaiting { action = (Obj.magic_many [@mode portable]) action; k })
;;
