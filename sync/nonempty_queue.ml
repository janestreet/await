open Base

type ('a : value mod contended portable) t = Cons of 'a * 'a t or_null

let[@inline] singleton x = Cons (x, Null)
let[@inline] dequeue (Cons (x, xs)) = #(x, xs)

let rec enqueue x (Cons (y, ys)) =
  match ys with
  | Null -> Cons (y, This (Cons (x, ys)))
  | This ys -> Cons (y, This (enqueue x ys))
;;

exception Not_found

let rec reject_exn x (Cons (y, ys)) =
  if phys_equal x y
  then ys
  else (
    match ys with
    | Null -> Exn.raise_without_backtrace Not_found
    | This ys -> This (Cons (y, reject_exn x ys)))
;;

let rec iter ~f = function
  | Cons (x, xs) ->
    f x;
    (match xs with
     | Null -> ()
     | This xs -> iter ~f xs)
;;
