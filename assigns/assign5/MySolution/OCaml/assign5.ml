#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<expr>  ::= <num> 
          | (add <exprs> )
          | (mul <exprs> )
<exprs> ::= <expr> | <expr><exprs>

*)

type expr =
  | Int of int       (* 1, 2, 3, 4 ...  *)
  | Add of expr list (* (add e1 e2 ...) *)
  | Mul of expr list (* (mul e1 e2 ...) *)

(* turn a string into a list of chars *)
let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)

(* remove blank chars at the front of a list *)
let rec trim cs =
  match cs with
  | [] -> cs
  | '\n' :: cs -> trim cs
  | '\t' :: cs -> trim cs
  | '\r' :: cs -> trim cs
  | ' ' :: cs -> trim cs
  | _ -> cs

(* Please implement a parse function. When given a valid string according
   to the grammar, your parse function returns an expr value encoding the
   expression.

   Example (Accpeted Strings):
   parse "(add 1 2 3)" = Some (Add [Int 1; Int 2; Int 3])
   parse "(mul (add 1 2) 3 (mul 1))" = Some (Mul [Add [Int 1; Int 2]; Int 3; Mul [Int 1]])

   Example (Rejected Strings):
   parse "()" = None
   parse "(add)" = None
   parse "(add 1 2))" = None
   parse "((mul 1 2)" = None

*)

let get_option_value opt default =
  match opt with
  | Some v -> v
  | None -> default

let parse_digit (c : char) : expr option = 
  match c with
  | '0'..'9' as digit -> Some (Int (int_of_char digit - int_of_char '0'))
  | _ -> None

let rec parse_num (cs : char list) : (expr * char list) option = 
  match cs with
  | x :: xs ->
    begin match parse_digit x with
    | Some (Int n) ->
        begin match parse_num xs with
        | Some (Int m, rest) -> Some (Int (n * 10 + m), rest)
        | None -> Some (Int n, xs)
        | _ -> None
        end
    | _ -> None
    end
  | _ -> None

let rec parse_exprs (cs : char list) : (expr list * char list) option = 
  match trim cs with
  | ')' :: rest -> Some ([], rest)
  | _ -> 
      match parse_expr cs with
      | Some (expr, rest) -> 
          begin match parse_exprs (trim rest) with
          | Some (exprs, rest') -> Some (expr :: exprs, rest')
          | None -> None
          end
      | None -> None

and parse_expr (cs : char list) : (expr * char list) option = 
  match trim cs with
  | '(' :: 'a' :: 'd' :: 'd' :: ' ' :: xs -> 
      begin match parse_exprs xs with
      | Some (exprs, ')' :: rest) -> Some (Add exprs, rest)
      | _ -> None
      end
  | '(' :: 'm' :: 'u' :: 'l' :: ' ' :: xs -> 
      begin match parse_exprs xs with
      | Some (exprs, ')' :: rest) -> Some (Mul exprs, rest)
      | _ -> None
      end
  | _ -> parse_num cs

let parse (s : string) : expr option = 
  match parse_expr (string_listize s) with
  | Some (exp, []) -> Some exp
  | _ -> None

