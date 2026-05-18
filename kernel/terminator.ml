open! Base
open! Portable_kernel
open Await_kernel_intf

let () =
  Stdlib.Printexc.Safe.register_printer (function
    | Terminated -> Some "Terminated"
    | _ -> None)
;;

type t = Cancellation0.t

let is_terminated = Cancellation0.is_canceled
let check t = if is_terminated t then raise Terminated
let same = Cancellation0.same
let unkillable = Cancellation0.never
let always = Cancellation0.always

module Source = struct
  type t = Cancellation0.Source.t

  let terminate = Cancellation0.Source.cancel
end

let is_terminatable = Cancellation0.is_cancellable
let source = Cancellation0.source
let with_ = Cancellation0.with_
let with_linked = Cancellation0.with_linked
let with_linked_multi = Cancellation0.with_linked_multi

module Link = struct
  type t =
    | Attached
    | Terminated
    | Signaled
  [@@deriving equal ~localize, sexp ~stackify]

  (* This should be the identity function in terms of the representations of
     [Cancellation0.Link.t] and [Terminator.Link.t]. *)
  let[@inline] of_cancellation : Cancellation0.Link.t -> t = function
    | Attached -> Attached
    | Canceled -> Terminated
    | Signaled -> Signaled
  ;;
end

let add_trigger t s = Link.of_cancellation (Cancellation0.add_trigger t s)

module Expert = struct
  include Cancellation0.Expert

  let cancellation t = t
end
