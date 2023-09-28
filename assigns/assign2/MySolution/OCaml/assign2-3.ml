#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../assign2.ml";;


let foldleft_to_iforeach (foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach =
  fun work xs ->
    let _ = foldleft xs 0 (fun _ x -> work x) in ()