open Base
open Portable

type t = int Atomic.t Iarray.t

(* *)

let bits_per_word = Int.num_bits - 1

let create n =
  let n_words = (n + (bits_per_word - 1)) / bits_per_word in
  Iarray.init n_words ~f:(fun _ -> Atomic.make 0)
;;

let get t i =
  let word_index = i / bits_per_word in
  let bit_index = i - (word_index * bits_per_word) in
  let bit_mask = 1 lsl bit_index in
  let word = Iarray.get t word_index in
  Atomic.get word land bit_mask = bit_mask
;;

let set t i v =
  let word_index = i / bits_per_word in
  let bit_index = i - (word_index * bits_per_word) in
  let bit_mask = 1 lsl bit_index in
  let word = Iarray.get t word_index in
  if v then Atomic.logor word bit_mask else Atomic.logand word (lnot bit_mask)
;;

let non_linearizable_pop t =
  (* Note that with the current maximum of 128 domains on 64-bit runtime there would be
     only 3 words in our intended use case of having one bit per domain. That means that
     this loop should run very quickly. *)
  let rec words word_index backoff =
    if word_index < Iarray.length t
    then (
      let word = Iarray.unsafe_get t word_index in
      let before = Atomic.get word in
      if before <> 0
      then (
        let bit_index = Int.ctz before in
        let bit_mask = 1 lsl bit_index in
        let after = before - bit_mask in
        match
          Atomic.compare_and_set word ~if_phys_equal_to:before ~replace_with:after
        with
        | Set_here ->
          let i = bit_index + (word_index * bits_per_word) in
          This i
        | Compare_failed ->
          (* We just retry from the same word.

             To make this operation (practically) linearizable we would e.g. need to store
             a version number in each word and increment that version number on each
             update (or otherwise ensure values are unique and we can detect ABA) and then
             retry if we notice any word has been updated during our pass through the
             bitset.

             It does not seem worth the trouble to make this linearizable for the use case
             which is to just quickly try to find an idle domain, because in that use case
             it doesn't strictly matter and the bitset is also likely to be relatively
             uncontended so we will likely skip set bits (i.e. idle domains) very
             rarely. *)
          words word_index (Backoff.once backoff))
      else words (word_index + 1) backoff)
    else Null
  in
  words 0 Backoff.default
;;
