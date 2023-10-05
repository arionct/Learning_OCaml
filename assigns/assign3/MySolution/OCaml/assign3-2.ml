#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let list_subsets (xs: 'a list): 'a list list =
  List.fold_left
    (fun acc x ->
      acc @ List.map (fun subset -> x :: subset) acc)
    [[]] xs
;;

let example_set = [1;2;3];;
let subsets = list_subsets example_set;;

(* Print the subsets *)
List.iter (fun subset -> List.iter (fun elem -> print_int elem; print_string " ") subset; print_newline ()) subsets;;