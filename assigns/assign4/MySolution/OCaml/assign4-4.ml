#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type 'a stream = Nil | Cons of 'a * (unit -> 'a stream)

let rec stream_append s1 s2 = match s1 with
  | Nil -> s2
  | Cons (x, f) -> Cons (x, (fun () -> stream_append (f ()) s2))

let rec stream_map f s = match s with
  | Nil -> Nil
  | Cons (x, g) -> Cons (f x, (fun () -> stream_map f (g ())))

let list_permute xs =
  let rec helper prefix suffix =
    match suffix with
    | [] -> Cons ([List.rev prefix], (fun () -> Nil))
    | _ ->
      let rec permute pref suff acc =
        match suff with
        | [] -> acc
        | hd :: tl ->
          let new_prefix = hd :: pref in
          let new_suffix = List.filter ((<>) hd) suff in
          let perms = helper new_prefix new_suffix in
          let perms_with_prefix = stream_map (fun x -> hd :: x) perms in
          permute pref tl (stream_append acc perms_with_prefix)
      in
      permute prefix suffix Nil
  in
  helper [] xs
