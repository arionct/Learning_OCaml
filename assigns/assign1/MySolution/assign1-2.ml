#use "./../../../classlib/OCaml/MyOCaml.ml";;

let string_make_fwork (n: int) (f: int -> char) : string =
  string_init n f

let string_merge (cs1: string) (cs2: string) : string =
  let len1 = string_length cs1 in
  let len2 = string_length cs2 in

  let f i =
    if i < len2 then
      string_get cs2 i
    else
      string_get cs1 (i - len2)
  in

  string_make_fwork (len1 + len2) f
