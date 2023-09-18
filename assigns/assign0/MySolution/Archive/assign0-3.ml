#use "./../assign0.ml"

let int2str(i0: int): string =
  let rec helper i acc =
    if i = 0 then
      acc
    else
      let digit = i mod 10 in
      let char_digit = chr (digit + 48) in
      helper (i / 10) (str char_digit ^ acc)
  in
  if i0 = 0 then "0" else helper i0 ""
