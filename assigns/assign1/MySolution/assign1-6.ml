#use "./../../../classlib/OCaml/MyOCaml.ml";;

let string_avoid_1324 (cs: string) : bool =
  let n = string_length cs in
  if n < 4 then true (* A string of length less than 4 is always 1324-avoid. *)
  else
    let rec loop i min_char max_char seen_mid1 seen_mid2 =
      if i >= n then true
      else
        let current_char = string_get_at cs i in
        if current_char < max_char && current_char > min_char then
          false
        else if current_char > max_char then
          loop (i + 1) min_char current_char seen_mid1 seen_mid2
        else
          let new_seen_mid1 = seen_mid1 || (current_char > min_char) in
          let new_seen_mid2 = seen_mid2 || (new_seen_mid1 && current_char > max_char) in
          loop (i + 1) min_char max_char new_seen_mid1 new_seen_mid2
    in
    loop 0 (string_get_at cs 0) (string_get_at cs 0) false false
;;

