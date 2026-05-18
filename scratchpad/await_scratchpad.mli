@@ portable

module Make (T : sig
    type t

    (** [create] may be called arbitrarily many times. *)
    val create : unit -> t @@ stateless
  end) : sig
  @@ portable
  (** [t] represents an ephemeral value of type [T.t] that may be accessed concurrently
      without contention. It is intended to replace static scratch space. [t] persists
      [shards] [T.t] value and provides a fresh value if the current shard is in use. *)
  type t : value mod contended non_float portable

  (** [create ~shards ()] is a scratchpad that persists at least [shards] values. *)
  val create : ?shards:int (** Default: 1 *) -> unit -> t

  (** Provides [uncontended] access to an ephemeral value of type [T.t]. *)
  val access
    :  t @ local
    -> f:(T.t -> 'a @ contended portable) @ local portable unyielding
    -> 'a @ contended portable

  (** Like [access], but returns unit. *)
  val iter : t @ local -> f:(T.t -> unit) @ local portable unyielding -> unit
end
