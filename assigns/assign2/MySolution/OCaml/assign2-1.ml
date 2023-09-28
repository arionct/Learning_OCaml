#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../assign2.ml";;

let rec mylist_length(xs: 'a mylist): int =
  match xs with
  | MyNil -> 0
  | MyCons(_, tail) -> 1 + mylist_length(tail)
  | MySnoc(init, _) -> mylist_length(init) + 1
  | MyReverse(lst) -> mylist_length(lst)
  | MyAppend2(lst1, lst2) -> mylist_length(lst1) + mylist_length(lst2)
;;