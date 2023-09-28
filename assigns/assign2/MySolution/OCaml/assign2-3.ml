#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../assign2.ml";;

let foldleft_to_iforeach (foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach =
  fun xs work ->
    foldleft xs 0 (fun _ idx x ->
      work idx x)
