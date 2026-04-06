(** Cancellation tokens with terminator-aware checking.

    This module re-exports the core functionality from {!Cancellation0} and adds
    terminator-aware versions of [is_canceled] and [check] that raise
    {!Terminator.Terminated} if the terminator has been terminated. *)

open! Base

type t = Cancellation0.t

let same = Cancellation0.same
let never = Cancellation0.never
let always = Cancellation0.always

module Source = Cancellation0.Source

let is_cancellable = Cancellation0.is_cancellable
let source = Cancellation0.source
let with_ = Cancellation0.with_
let with_linked = Cancellation0.with_linked
let with_linked_multi = Cancellation0.with_linked_multi

module Link = Cancellation0.Link

let add_trigger = Cancellation0.add_trigger

module Expert = struct
  include Cancellation0.Expert

  let is_canceled_ignore_termination = Cancellation0.is_canceled
end

module For_testing = Cancellation0.For_testing

let is_canceled c ~terminator =
  Terminator.check terminator;
  Cancellation0.is_canceled c
;;

let check c ~terminator : unit Or_canceled.t =
  Terminator.check terminator;
  if Cancellation0.is_canceled c then Canceled else Completed ()
;;
