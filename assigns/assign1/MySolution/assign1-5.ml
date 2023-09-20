#use "./../../../classlib/OCaml/MyOCaml.ml";;

let string_longest_ascend(xs: string): string = 
  let rec helper (xs: string) (ys: string) (zs: string) (max: string): string =
    match xs with
    | "" -> max
    | _ -> 
      let x = String.get xs 0 in
      let xs' = String.sub xs 1 (String.length xs - 1) in
      if ys = "" then helper xs' (String.make 1 x) (String.make 1 x) max
      else if x <= String.get ys (String.length ys - 1) then helper xs' (ys ^ (String.make 1 x)) (String.make 1 x) max
      else if String.length ys > String.length max then helper xs' (ys ^ (String.make 1 x)) (String.make 1 x) ys
      else helper xs' (ys ^ (String.make 1 x)) (String.make 1 x) max
  in
  helper xs "" "" ""