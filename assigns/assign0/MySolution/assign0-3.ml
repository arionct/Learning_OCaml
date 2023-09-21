#use "./../assign0.ml";;

let char_string_concat (c: char) (s: string) : string =
  let len = string_length s in
  string_init (len + 1) (fun i ->
    if i = 0 then
      chr (ord c)
    else
      string_get (s, i - 1)
  )

let int2str(i0: int): string =
  let rec helper i acc =
    if i = 0 then
      acc
    else
      let digit = i mod 10 in
      let char_digit = chr (digit + 48) in
      helper (i / 10) (char_string_concat char_digit acc)
  in
  if i0 = 0 then "0" else
  if i0 < 0 then char_string_concat '-' (helper (abs i0) "")
  else helper i0 ""