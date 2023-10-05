#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let string_concat(s1: string)(s2: string): string =
  let len1 = string_length s1 in
  let len2 = string_length s2 in
  let result = string_init (len1 + len2) (fun i ->
    if i < len1 then string_get_at s1 i
    else string_get_at s2 (i - len1)
  )
  in result
;;

let list_of_buddies(word: string): string list =
  let n = string_length word in
  let buddy_at_i i c =
    let before = string_init i (string_get_at word) in
    let after = string_tabulate (n-i-1) (fun j -> string_get_at word (i+j+1)) in
    string_concat before (string_concat (str c) after)
  in
  let buddies = ref [] in
  for i = 0 to n-1 do
    let current_char = string_get_at word i in
    for c = int_of_char 'a' to int_of_char 'z' do
      if c <> (int_of_char current_char) then
        buddies := (buddy_at_i i (char_of_int c)) :: !buddies
    done
  done;
  !buddies
;;

