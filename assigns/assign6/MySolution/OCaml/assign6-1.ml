#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(* ****** ****** *)
(*
//
Assign6:
Parsing and parsing combinators
//
DUE: the 13th of November, 2023
//
Except for the basic arithmetic functions
(including those on chars), you may only use
the functions in classlib/OCaml/MyOCaml.ml
//
*)
(* ****** ****** *)

(*
//
Assign6-1:
//
Please implement a print and parse function. Using parser combinators. When
given a valid string according to the grammar, your parse function returns an
sexpr value encoding the expression.

//
let sexpr_to_string (e : sexpr)  : string       = ...
let sexpr_parse     (s : string) : sexpr option = ...
//

Example (Accepted Strings):
parse "(add 1 2 3)" = Some (Add [Int 1; Int 2; Int 3])
parse "(mul (add 1 2) 3 (mul 1))" = Some (Mul [Add [Int 1; Int 2]; Int 3; Mul [Int 1]])
//
Example (Rejected Strings):
parse "()" = None
parse "(add)" = None
parse "(add 1 2))" = None
parse "((mul 1 2)" = None
//
*)

(* ****** ****** *)

(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<exprs> ::= <expr> | <expr> <exprs>
<expr>  ::= <num>
          | (add <exprs> )
          | (mul <exprs> )
*)

type sexpr =
  | SInt of int        (* 1, 2, 3, 4 ...  *)
  | SAdd of sexpr list (* (add e1 e2 ...) *)
  | SMul of sexpr list (* (mul e1 e2 ...) *)

(* ****** ****** *)

(* end of [CS320-2023-Fall-assigns-assign6.ml] *)


let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)

let rec trim cs =
  match cs with
  | [] -> cs
  | '\n' :: cs -> trim cs
  | '\t' :: cs -> trim cs
  | '\r' :: cs -> trim cs
  | ' ' :: cs -> trim cs
  | _ -> cs

let rec parse_digits cs =
  match cs with
  | [] -> ([], cs)
  | c :: cs ->
    let x = ord c - ord '0' in
    if 0 <= x && x <= 9 then
      let xs, cs = parse_digits cs in
      (x :: xs, cs) 
    else ([], c :: cs)

let parse_int cs =
  let xs, cs = parse_digits cs in
  match xs with
  | [] -> None
  | _ ->
    let n = list_foldleft xs 0 (fun acc x -> acc * 10 + x) in
    Some (SInt n, cs)

let parse_str s cs =
  let cs0 = string_listize s in
  let rec loop cs cs0 =
    match cs, cs0 with
    | c :: cs, c0 :: cs0 -> 
      if c = c0
      then loop cs cs0
      else None
    | _, [] -> Some cs
    | _ -> None
  in loop cs cs0

let attempt ps cs =
  list_foldleft ps None
    (fun acc p ->
       match acc with
       | Some _ -> acc
       | None -> p cs)

let rec parse_sexpr cs =
  attempt [parse_int; parse_add; parse_mul] cs

and parse_sexprs cs =
  match parse_sexpr cs with
  | None -> None
  | Some (e, cs) -> 
    match parse_sexprs (trim cs) with
    | Some (es, cs) -> Some (e :: es, cs)
    | None -> Some ([e], cs)

and parse_add cs =
  match parse_str "(add" cs with
  | None -> None
  | Some cs ->
    match parse_sexprs (trim cs) with
    | Some (es, ')' :: cs) -> Some (SAdd es, cs)
    | _ -> None

and parse_mul cs =
  match parse_str "(mul" cs with
  | None -> None
  | Some cs ->
    match parse_sexprs (trim cs) with
    | Some (es, ')' :: cs) -> Some (SMul es, cs)
    | _ -> None

let sexpr_parse (s : string) : sexpr option =
  let cs = string_listize s in
  match parse_sexpr cs with
  | Some (e, []) -> Some e
  | _ -> None


  
let rec int_to_string (n: int) : string =
  if n < 10 then str (char_of_digit n)
  else int_to_string (n / 10) ^ str (char_of_digit (n mod 10))

let rec concat (s1: string) (s2: string) : string =
  match string_listize s1 with
  | [] -> s2
  | c :: cs -> str c ^ concat (list_tl cs) s2

and sexprs_to_string (es: sexpr list) : string =
  match es with
  | [] -> ""
  | [e] -> sexpr_to_string e
  | e :: es -> concat (sexpr_to_string e) (concat " " (sexprs_to_string es))

and sexpr_to_string (e: sexpr) : string =
  match e with
  | SInt n -> int_to_string n
  | SAdd es -> concat "(add " (concat (sexprs_to_string es) ")")
  | SMul es -> concat "(mul " (concat (sexprs_to_string es) ")")