#use "./../../../../classlib/OCaml/MyOCaml.ml";;


type 'a stream =
  | StrNil
  | StrCons of 'a * (unit -> 'a stream)

let the_ln2_stream =
  let rec partial_sums n sum sign =
    let term = (1.0 /. float_of_int n) *. sign in
    let new_sum = sum +. term in
    StrCons(new_sum, fun () -> partial_sums (n+1) new_sum (sign *. -1.0))
  in
  fun () -> partial_sums 1 0.0 1.0