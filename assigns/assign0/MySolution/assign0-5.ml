#use "./../assign0.ml";;

let stringrev(cs: string): string =
  let len = string_length cs in
  let result = string_init len (fun i -> string_get (cs, len - i - 1)) in
  result