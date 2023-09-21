#use "./../../../classlib/OCaml/MyOCaml.ml";;

let string_sub (cs: string) (start: int) (len: int) : string =
  let n = string_length cs in
  let end_idx = min (start + len) n in
  String.init (end_idx - start) (fun i -> String.get cs (start + i))
;;

let intrep_add (ds1: string) (ds2: string) : string =
  let n1 = string_length ds1 in
  let n2 = string_length ds2 in
  let max_len = max n1 n2 in

  let add_digit d1 d2 carry =
    let sum = digit_of_char d1 + digit_of_char d2 + carry in
    let new_digit = char_of_digit (sum mod 10) in
    let new_carry = sum / 10 in
    (new_digit, new_carry)
  in

  let rec add_digits i carry result =
    if i >= max_len then
      if carry > 0 then
        string_cons (char_of_digit carry) result
      else
        result
    else
      let d1 = if i < n1 then string_get_at ds1 (n1 - i - 1) else '0' in
      let d2 = if i < n2 then string_get_at ds2 (n2 - i - 1) else '0' in
      let (new_digit, new_carry) = add_digit d1 d2 carry in
      add_digits (i + 1) new_carry (string_cons new_digit result)
  in

  let result = add_digits 0 0 "" in

  if string_get_at result 0 = '0' && string_length result > 1 then
    string_sub result 1 (string_length result - 1)
  else
    result
;;