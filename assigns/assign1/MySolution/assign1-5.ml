#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*
assign1-5: 20 points

A sequence of chars is ascending if any char in
the sequence is less than or equal to the following
one (when the following one does exist).
Given a string cs, please implement a function
that find the longest ascending subsequence of [cs].
If there are more than one such sequences, the left
most one should be returned.

fun string_longest_ascend(xs: string): string

For instance, given "1324561111", the function
string_longest_ascend returns "13456"

For instance, given "1234561111", the function
string_longest_ascend returns "123456"

For instance, given "1234511111", the function
string_longest_ascend returns "111111".
*)

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