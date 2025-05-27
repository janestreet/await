open Async
open Await
open Portable

type 'a op = Await : Trigger.t -> unit op

module Eff = struct
  include Effect.Make (struct
      type 'a t = 'a op
    end)

  let rec handle = function
    | Value value -> value
    | Exception e -> raise e
    | Operation (Await trigger, k) ->
      let open struct
        external magic_unique : 'a -> 'a @ unique @@ portable = "%identity"
      end in
      let continue _ = handle (Effect.continue (magic_unique k) () []) in
      let continue_capsule = Capsule.Initial.Data.wrap continue in
      let context = Scheduler.current_execution_context () in
      let context_capsule = Capsule.Initial.Data.wrap context in
      if not
           (Trigger.on_signal trigger (fun () ->
              Async_kernel_scheduler.portable_enqueue_job context_capsule continue_capsule))
      then handle (Effect.continue (magic_unique k) () [])
  ;;
end

let await handler trigger =
  Eff.perform
    (Capsule.Expert.Data.Local.unwrap ~access:Capsule.Expert.initial handler)
    (Await trigger) [@nontail]
;;

module Expert = struct
  let with_await terminator ~f =
    let terminator = Terminator.Expert.globalize terminator in
    Eff.handle
      ((Eff.run [@alert "-experimental"]) (fun handler ->
         let handler = (Capsule.Initial.Data.wrap [@mode local]) handler in
         let await = Await.create terminator ~await handler in
         f await [@nontail]))
  ;;

  let thread_safe_spawn context action =
    let action = Unique.Once.make action in
    Async_kernel_scheduler.thread_safe_enqueue_job
      context
      (fun () -> (Unique.Once.get_exn action) ())
      ()
  ;;
end

let schedule_with_await ?monitor ?priority terminator ~f =
  let f = Unique.Once.make f in
  Deferred.create (fun ivar ->
    schedule ?monitor ?priority (fun () ->
      Expert.with_await terminator ~f:(fun w ->
        match (Unique.Once.get_exn f) w with
        | value -> Ivar.fill_exn ivar value
        | exception exn -> Monitor.send_exn (Monitor.current ()) exn)
      [@nontail]))
;;

let await_deferred t deferred =
  if Deferred.is_determined deferred
  then Deferred.value_exn deferred
  else (
    let trigger = Trigger.create () in
    Deferred.upon deferred (fun _value -> Trigger.Source.signal (Trigger.source trigger));
    await_until_terminated t trigger;
    if Deferred.is_determined deferred
    then Deferred.value_exn deferred
    else raise Terminated)
;;
