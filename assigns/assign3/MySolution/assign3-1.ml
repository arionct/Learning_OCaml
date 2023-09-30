#use "./../../../classlib/OCaml/MyOCaml.ml";;


let rec matrix_transpose(xss: 'a list list): 'a list list =
  match xss with
  | [] -> []
  | [] :: _ -> []
  | _ ->
    let head_column = List.map List.hd xss in
    let tail_transposed = matrix_transpose (List.map List.tl xss) in
    head_column :: tail_transposed
;;