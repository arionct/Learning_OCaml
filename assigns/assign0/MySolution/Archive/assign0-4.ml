#use "./../assign0.ml";;

let rec power10 n = 
  if n = 0 then 1 else 10 * power10 (n - 1)

let rec char_to_digit c = 
  let ascii_value = ord c in
  ascii_value - 48

let str2int(cs: string): int =
  let len = string_length cs in
  let rec helper i acc =
    if i < len then
      let digit = char_to_digit (string_get (cs, i)) in
      helper (i + 1) (acc * 10 + digit)
    else
      acc
  in
  helper 0 0
