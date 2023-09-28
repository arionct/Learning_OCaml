#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../assign2.ml";;

let string_sepjoin_list (sep: string) (xs: string list): string =
  list_foldleft xs "" (fun acc s -> if acc = "" then s else acc ^ sep ^ s)
