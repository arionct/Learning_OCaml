#use "./../../../classlib/OCaml/MyOCaml.ml";;

let string_make_fwork (cs1: string) (cs2: string) : (int -> char) =
  let n1 = string_length cs1 in
  fun i ->
    if i < n1 then string_get_at cs1 i
    else string_get_at cs2 (i - n1)
;;

let string_merge (cs1: string) (cs2: string) : string =
  let n = string_length cs1 + string_length cs2 in
  let fwork = string_make_fwork cs1 cs2 in
  string_tabulate n fwork
;;