#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let list_map = fun xs -> foreach_to_map_list(list_foreach)(xs)

let list_head : 'a list -> 'a = fun xs -> match xs with
  | [] -> failwith "list_head"
  | x :: _ -> x

let list_tail : 'a list -> 'a list = fun xs -> match xs with
  | [] -> failwith "list_tail"
  | _ :: xs' -> xs'

let rec matrix_transpose (xss: 'a list list) : 'a list list =
  match xss with
  | [] -> []
  | [] :: _ -> []
  | _ -> (list_map (fun x -> [list_head x]) xss) :: matrix_transpose (list_map (fun x -> list_tail x) xss)
;;