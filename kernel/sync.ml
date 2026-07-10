open! Base
open! Portable_kernel

type t =
  | T :
      ('c : value_or_null mod contended portable).
      { context : 'c
      ; sync : #('c * Trigger.t global) @ local -> unit @@ portable
      ; yield : ('c @ local -> unit) or_null @@ portable
      }
      -> t

let with_ ~yield ~sync context ~f = exclave_
  let (P access) = Capsule.Access.current () in
  let sync = (Capsule.Data.wrap [@mode local]) ~access sync in
  let yield =
    match yield with
    | Null -> Null
    | This yield -> This ((Capsule.Data.wrap [@mode local]) ~access yield)
  in
  let context = (Capsule.Data.wrap [@mode local]) ~access context in
  (Capsule.Prim.Password.with_current [@mode local]) access (fun password -> exclave_
    f
      (T
         { context
         ; sync =
             (fun #(ctx, tgr) ->
               Capsule.Prim.Data.Local.iter
                 ~password
                 ((Capsule.Data.both [@mode local]) ctx sync)
                 ~f:(fun (ctx, sync) -> sync #(ctx, tgr))
               [@nontail])
         ; yield =
             (match yield with
              | Null -> Null
              | This yield ->
                This
                  (fun ctx ->
                    Capsule.Prim.Data.Local.iter
                      ~password
                      ((Capsule.Data.both [@mode local]) ctx yield)
                      ~f:(fun (ctx, yield) -> yield ctx)
                    [@nontail]))
         }) [@nontail])
;;

let%template create ~yield ~sync context =
  T
    { yield =
        (match yield with
         | Null -> Null
         | This yield -> This (fun { portended = context } -> yield context [@nontail]))
    ; sync
    ; context = { portended = context }
    }
  [@exclave_if_stack a]
[@@alloc a @ l = (stack_local, heap_global)]
;;

let call_sync (T { sync; context; yield = _ }) trigger =
  sync #(context, { global = trigger })
;;

let yield (T { yield; context; sync = _ }) =
  match yield with
  | Null -> ()
  | This yield -> yield context
;;

module For_testing = struct
  let never =
    let yield =
      (* We intentionally allow [yield], i.e. do not raise from it. [yield] provides a
         hint to the scheduler that "I could be more fair here, if the scheduler wanted me
         to be" and is different enough from (un)bounded await for some explicit signal. *)
      Null
    in
    let sync #({ portended = () }, { global = (_ : Trigger.t) }) =
      failwith
        "[sync never] was called. Usually this means that an operation blocked which was \
         expected to never block"
    in
    create ~yield ~sync ()
  ;;
end

module Expert = struct
  let sync_without_checking_trigger t ~on:trigger = call_sync t trigger

  let sync t ~on:trigger =
    if not (Trigger.is_signalled trigger) then sync_without_checking_trigger t ~on:trigger
  ;;

  let sync_or_cancel t cancellation ~on:trigger =
    if not (Trigger.is_signalled trigger)
    then (
      match Cancellation.add_trigger cancellation (Trigger.source trigger) with
      | Attached -> call_sync t trigger
      | Canceled -> Trigger.Source.signal (Trigger.source trigger)
      | Signaled -> ())
  ;;
end
