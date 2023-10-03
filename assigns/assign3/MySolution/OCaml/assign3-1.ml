#use "./../../../../classlib/OCaml/MyOCaml.ml";;


let rec matrix_transpose(xss: 'a list list): 'a list list =
  match xss with
  | [] -> [] (* If the matrix is empty, return an empty matrix *)
  | [] :: _ -> [] (* If any of the inner lists is empty, return an empty matrix *)
  | _ ->
    let head_column = foreach_to_listize(list_foreach)(xss)(fun xs -> List.hd xs) in
    let tail_matrix = foreach_to_listize(list_foreach)(xss)(fun xs -> List.tl xs) in
    head_column :: matrix_transpose(tail_matrix)
;;
