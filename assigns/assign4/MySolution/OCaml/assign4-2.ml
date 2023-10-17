#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type 'a stream = unit -> 'a strcon
and 'a strcon = StrNil | StrCons of 'a * 'a stream;;

let theNatPairs: (int*int) stream = 
  let rec helper sum i j =
    if i + j = sum then
      StrCons ((i, j), fun () -> if j = 0 then helper (sum + 1) (sum + 1) 0 else helper sum i (j - 1))
    else
      StrCons ((i, j), fun () -> helper sum (i - 1) (j + 1))
  in
  fun () -> helper 0 0 0
