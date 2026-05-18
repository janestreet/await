@@ portable

(** A poisonable mutual exclusion lock. *)

module Sync : Mutex_common.Sync (** @open *)

module Await : Mutex_common.Await (** @open *)
