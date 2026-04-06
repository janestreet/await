open! Base
open! Portable_kernel

type t = Sync.t

let of_sync sync = sync
let yield = Sync.yield
