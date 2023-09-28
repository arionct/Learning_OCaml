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

let rec mylist_get_at(xs: 'a mylist)(i0: int): 'a =
  match xs, i0 with
  | MyCons(x, _), 0 -> x
  | MyCons(_, tail), _ -> mylist_get_at(tail)(i0 - 1)
  | MySnoc(_, x), 0 -> x
  | MySnoc(init, _), _ -> mylist_get_at(init)(i0 - 1)
  | MyReverse(lst), _ -> mylist_get_at(lst)(mylist_length(lst) - 1 - i0)
  | MyAppend2(lst1, lst2), _ ->
      let len1 = mylist_length(lst1) in
      if i0 < len1 then mylist_get_at(lst1)(i0)
      else mylist_get_at(lst2)(i0 - len1)
  | _ -> raise MySubscript
;;