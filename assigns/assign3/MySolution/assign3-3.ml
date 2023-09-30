#use "./../../../classlib/OCaml/MyOCaml.ml";;

let list_nchoose (xs: 'a list) (n0: int): 'a list list =
  let combine acc x =
    acc @ List.map (fun subset -> x :: subset) acc
  in
  List.fold_left combine [[]] xs
  |> List.filter (fun subset -> List.length subset = n0)
;;