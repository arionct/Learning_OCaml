#use "./../../../classlib/OCaml/MyOCaml.ml";;

let string_avoid_132 (cs: string) : bool =
  let n = string_length cs in
  if n < 3 then true (* A string of length less than 3 is always 132-avoid. *)
  else
    let rec loop i min_char max_char seen_mid =
      if i >= n then true
      else
        let current_char = string_get_at cs i in
        if current_char < max_char && current_char > min_char then
          false
        else if current_char > max_char then
          loop (i + 1) min_char current_char true
        else
          loop (i + 1) min_char max_char seen_mid
    in
    loop 0 (string_get_at cs 0) (string_get_at cs 0) false
;;