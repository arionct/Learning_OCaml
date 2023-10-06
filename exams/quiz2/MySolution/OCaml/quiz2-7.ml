(* ************************************************ *)

(*
Q2-7: 10 points

The following implementation of list_append is not tail-recursive.
Please give an implementation of list_append that is tail-recursive.

Note that you can only use pattern matching and list_foldleft in your
implementation.
 
let rec
list_append(xs: 'a list)(ys: 'a list) =
match xs with
  [] -> ys | x1 :: xs -> x1 :: list_append(xs)(ys)
*)

(* ************************************************ *)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let list_append(xs: 'a list): 'a list = 
  let fold_fn acc x = x :: acc in
  let reversed_xs = list_foldleft fold_fn xs [] in
  list_foldleft fold_fn ys (list_foldleft fold_fn reversed_xs [])