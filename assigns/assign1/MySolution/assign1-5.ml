#use "./../../../classlib/OCaml/MyOCaml.ml";;

let string_longest_ascend xs =
  let n = string_length xs in
  if n = 0 then ""
  else
    let res = ref "" in
    for i = 0 to n - 1 do
      let cur = ref (str (string_get_at xs i)) in
      let last_char = ref (string_get_at xs i) in
      for j = i + 1 to n - 1 do
        let ch = string_get_at xs j in
        if ch >= !last_char then (cur := !cur ^ str ch; last_char := ch)
      done;
      if string_length !cur > string_length !res then res := !cur
    done;
    !res
;;