@@ portable

open Basement
open Await_kernel

type 'k t : value mod contended portable

val create : unit -> 'k t

val wait
  :  acquire:(Await.t @ local -> 'lock @ local -> unit) @ once portable
  -> release:('lock @ local -> unit) @ once portable
  -> Await.t @ local
  -> 'k t @ local
  -> lock:'lock @ local
  -> 'k Capsule.Key.t @ unique
  -> 'k Capsule.Key.t @ unique

val signal : 'k t @ local -> unit
val broadcast : 'k t @ local -> unit
