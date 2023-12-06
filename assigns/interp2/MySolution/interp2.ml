#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-2.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

(* ------------------------------------------------------------ *)

(* abstract syntax tree of interp2 *)

type const =
   | Int of int
   | Bool of bool
   | Unit
   | Sym of string
   | Closure of string * (string * const) list * com list

and com =
   | Push of const
   | Pop 
   | Swap 
   | Trace
   | Add 
   | Sub 
   | Mul 
   | Div
   | And 
   | Or 
   | Not
   | Lt 
   | Gt
   | If of com list * com list
   | Bind 
   | Lookup
   | Fun of com list 
   | Call 
   | Return 

(* ------------------------------------------------------------ *)

(* parsers for interp2 *)

let parse_natural = 
   let* n = natural << whitespaces in pure n

let parse_int =
   (let* n = parse_natural in pure (Int n)) <|>
   (keyword "-" >> let* n = parse_natural in pure (Int (-n)))

let parse_bool =
   (keyword "True" >> pure (Bool true)) <|>
   (keyword "False" >> pure (Bool false))

let parse_unit =
   keyword "Unit" >> pure Unit
   
let letter_or_number c =
   (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

let check c =
   match c with
   | '\n' -> "\\n"
   | '\t' -> "\\t"
   | '\r' -> "\\r"
   | '\\' -> "\\\\"
   | '\'' -> "\\'"
   | _    -> str c

let ch =
   let* chars = many (satisfy letter_or_number) in
   pure (list_make_fwork (fun work -> list_foreach chars work))

let parse_str : string parser =
   fun ls ->
      match ch ls with
      | Some (charList, rest) ->
         Some (list_foldleft charList "" (fun acc c -> acc ^ check c), rest)
      | None -> None

let parse_symbol = 
   let* n = parse_str << whitespaces in pure (Sym n)
    
let parse_const =
   parse_int <|>
   parse_bool <|>
   parse_unit <|>
   parse_symbol
 
let rec parse_com () = 
   (keyword "Push" >> parse_const >>= fun c -> pure (Push c)) <|>
   (keyword "Pop" >> pure Pop) <|>
   (keyword "Swap" >> pure Swap) <|>
   (keyword "Trace" >> pure Trace) <|>
   (keyword "Add" >> pure Add) <|>
   (keyword "Sub" >> pure Sub) <|>
   (keyword "Mul" >> pure Mul) <|>
   (keyword "Div" >> pure Div) <|>
   (keyword "And" >> pure And) <|>
   (keyword "Or" >> pure Or) <|>
   (keyword "Not" >> pure Not) <|>
   (keyword "Lt" >> pure Lt) <|>
   (keyword "Gt" >> pure Gt) <|>
   (parse_if_else ()) <|>
   (keyword "Bind" >> pure Bind) <|>
   (keyword "Lookup" >> pure Lookup) <|>
   (parse_function ()) <|>
   (keyword "Call" >> pure Call) <|>
   (keyword "Return" >> pure Return)
 
 and parse_if_else () =
   let* _ = keyword "If" in
   let* c1 = parse_coms () in
   let* _ = keyword "Else" in
   let* c2 = parse_coms () in
   let* _ = keyword "End" in
   pure (If (c1, c2))
 and parse_function () = 
   let* _ = keyword "Fun" in
   let* c = parse_coms () in
   let* _ = keyword "End" in
   pure (Fun (c))
and parse_coms () = many' (fun x -> parse_com x << keyword ";")
 
(* ------------------------------------------------------------ *)

(* interpreter *)

type coms = com list
type stack = const list
type trace = string list
type env = (string * const) list
type prog = coms

let rec str_of_natural n =
   let d = n mod 10 in 
   let n0 = n / 10 in
   let s = str (chr (d + ord '0')) in 
   if 0 < n0 then
      string_append (str_of_natural n0) s
   else s

let str_of_int n = 
   if n < 0 then
      string_append "-" (str_of_natural (-n))
   else str_of_natural n

let str_of_function c = 
   "Fun<" ^ c ^ ">"

let toString c =
   match c with
   | Int i -> str_of_int i
   | Bool true -> "True"
   | Bool false -> "False"
   | Unit -> "Unit"
   | Sym s -> s
   | Closure (x, _, _) -> str_of_function x

let rec custom_lookup (key: 'a) (env: ('a * 'b) list): 'b option =
   match env with
   | [] -> None
   | (k, v) :: tail -> 
      if k = key then Some v 
      else custom_lookup key tail
 
let rec eval (stack : stack) (trace : trace) (env: env) (prog : prog) : trace =
   match prog with
   | [] -> trace  (* Termination state *)
   | Push c :: p0 -> eval (c :: stack) trace env p0     (* PushStack *)
   | Pop :: p0 -> (
      match stack with
      | _ :: s0   -> eval s0 trace env p0               (* PopStack *)
      | []        -> eval [] ("Panic" :: trace) env []  (* PopError *)
   )
   | Swap :: p0 -> (
      match stack with 
      | c1 :: c2 :: s0 -> eval (c2 :: c1 :: s0) trace env p0  (* SwapStack *)
      | []             -> eval [] ("Panic" :: trace) env []   (* SwapError1 *)
      | _ :: []        -> eval [] ("Panic" :: trace) env []   (* SwapError2 *)
   )
   | Trace :: p0 -> (
      match stack with
      | c :: s0 -> eval (Unit :: s0) (toString c :: trace) env p0    (* TraceStack *)
      | []      -> eval [] ("Panic" :: trace) env []                 (* TraceError *)
   )
   | Add :: p0 -> (
      match stack with
      | Int i :: Int j :: s0 -> eval (Int (i + j) :: s0) trace env p0   (* AddStack *)
      | _ :: _ :: s0         -> eval [] ("Panic" :: trace) env []       (* AddError1 *)
      | []                   -> eval [] ("Panic" :: trace) env []       (* AddError2 *)
      | _ :: []              -> eval [] ("Panic" :: trace) env []       (* AddError3 *)
   )
   | Sub :: p0 -> (
      match stack with
      | Int i :: Int j :: s0 -> eval (Int (i - j) :: s0) trace env p0   (* SubStack *)
      | _ :: _ :: s0         -> eval [] ("Panic" :: trace) env []       (* SubError1 *)
      | []                   -> eval [] ("Panic" :: trace) env []       (* SubError2 *)
      | _ :: []              -> eval [] ("Panic" :: trace) env []       (* SubError3 *)
   )
   | Mul :: p0 -> (
      match stack with
      | Int i :: Int j :: s0 -> eval (Int (i * j) :: s0) trace env p0   (* MulStack *)
      | _ :: _ :: s0         -> eval [] ("Panic" :: trace) env []       (* MulError1 *)
      | []                   -> eval [] ("Panic" :: trace) env []       (* MulError2 *)
      | _ :: []              -> eval [] ("Panic" :: trace) env []       (* MulError3 *)
   )
   | Div :: p0 -> (
      match stack with
      | Int i :: Int 0 :: s0 -> eval [] ("Panic" :: trace) env []       (* DivError0 *)
      | Int i :: Int j :: s0 -> eval (Int (i / j) :: s0) trace env p0   (* DivStack *)
      | _ :: _ :: s0         -> eval [] ("Panic" :: trace) env []       (* DivError1 *)
      | []                   -> eval [] ("Panic" :: trace) env []       (* DivError2 *)
      | _ :: []              -> eval [] ("Panic" :: trace) env []       (* DivError3 *)
   )
   | And :: p0 -> (
      match stack with
      | Bool a :: Bool b :: s0 -> eval (Bool (a && b) :: s0) trace env p0  (* AndStack *)
      | _ :: _ :: s0           -> eval [] ("Panic" :: trace) env []        (* AndError1 *)
      | []                     -> eval [] ("Panic" :: trace) env []        (* AndError2 *)
      | _ :: []                -> eval [] ("Panic" :: trace) env []        (* AndError3 *)
   )
   | Or :: p0 -> (
      match stack with
      | Bool a :: Bool b :: s0 -> eval (Bool (a || b) :: s0) trace env p0  (* OrStack *)
      | _ :: _ :: s0           -> eval [] ("Panic" :: trace) env []        (* OrError1 *)
      | []                     -> eval [] ("Panic" :: trace) env []        (* OrError2 *)
      | _ :: []                -> eval [] ("Panic" :: trace) env []        (* OrError3 *)
   )
   | Not :: p0 -> (
      match stack with
      | Bool a :: s0 -> eval (Bool (not a) :: s0) trace env p0  (* NotStack *)
      | _ :: s0      -> eval [] ("Panic" :: trace) env []       (* NotError1 *)
      | []           -> eval [] ("Panic" :: trace) env []       (* NotError2 *)
   )
   | Lt :: p0 -> (
      match stack with
      | Int i :: Int j :: s0 -> eval (Bool (i < j) :: s0) trace env p0   (* LtStack *)
      | _ :: _ :: s0         -> eval [] ("Panic" :: trace) env []        (* LtError1 *)
      | []                   -> eval [] ("Panic" :: trace) env []        (* LtError2 *)
      | _ :: []              -> eval [] ("Panic" :: trace) env []        (* LtError3 *)
   )
   | Gt :: p0 -> (
      match stack with
      | Int i :: Int j :: s0 -> eval (Bool (i > j) :: s0) trace env p0   (* GtStack *)
      | _ :: _ :: s0         -> eval [] ("Panic" :: trace) env []        (* GtError1 *)
      | []                   -> eval [] ("Panic" :: trace) env []        (* GtError2 *)
      | _ :: []              -> eval [] ("Panic" :: trace) env []        (* GtError3 *)
   )
   | If (c1, c2) :: p0 -> (
      match stack with
      | Bool b :: s0 -> (
         if b then
         match prog with
         | If (c1, _) :: rest_prog -> eval s0 trace env (c1 @ rest_prog)  (* IfTrue *)
         | _                       -> eval [] ("Panic" :: trace) env []   (* IfError1 *)
         else 
         match prog with
         | If (_, c2) :: rest_prog -> eval s0 trace env (c2 @ rest_prog)  (* IfFalse *)
         | _                       -> eval [] ("Panic" :: trace) env []   (* IfError2 *)
      )
      | _                          -> eval [] ("Panic" :: trace) env []   (* IfError3 *)
   )
   | Bind :: p0 -> (
      match stack with
      | Sym x :: v :: s0 -> eval s0 trace ((x, v) :: env) p0   (* BindStack *)
      | _                -> eval [] ("Panic" :: trace) env []  (* BindError *)
   )
   | Lookup :: p0 -> (
      match stack with
      | Sym x :: s0 -> (
         match custom_lookup (x) (env) with
         | Some v -> eval (v :: s0) trace env p0        (* LookupStack *)
         | _      -> eval [] ("Panic" :: trace) env []  (* LookupError *)
      )
      | _         -> eval [] ("Panic" :: trace) env []  (* LookupError *)
   )
   | Fun c :: p0 -> (
      match stack with
      | Sym x :: s0 ->
         let closure = Closure (x, env, c) in
         eval (closure :: s0) trace env p0      (* FunStack *)
      | _ -> eval [] ("Panic" :: trace) env []  (* FunError *)
   )
   | Call :: p0-> (
      match stack with
      | Closure (f, vf, c) :: a :: s0 ->
         let new_env = (f, Closure (f, vf, c)) :: vf in
         let cc_closure = Closure ("cc", env, p0) in
         eval (a :: cc_closure :: s0) trace new_env c  (* CallStack *)
      | _ -> eval [] ("Panic" :: trace) env []         (* CallError *)
   )
   | Return :: p0 -> (
      match stack with
      | Closure (f, vf, c) :: a :: s0 ->
         let new_env = vf in
         eval (a :: s0) trace new_env c         (* ReturnStack *)
      | _ -> eval [] ("Panic" :: trace) env []  (* ReturnError *)
   )

(* ------------------------------------------------------------ *)

(* putting it all together [input -> parser -> eval -> output] *)

let interp (s : string) : string list option =
   match string_parse (whitespaces >> parse_coms ()) s with
   | Some (p, []) -> Some (eval [] [] [] p)
   | _ -> None

(* ------------------------------------------------------------ *)

(* interp from file *)

let read_file (fname : string) : string =
  let fp = open_in fname in
  let s = string_make_fwork (fun work ->
      try
        while true do
          work (input_char fp)
        done
      with _ -> ())
  in
  close_in fp; s

let interp_file (fname : string) : string list option =
  let src = read_file fname in
  interp src

(* ------------------------------------------------------------ *)

(* tests *)

let test_polynomial () =
   let program = "Push 3; Push 3; Mul; Push -4; Push 3; Mul; Add; Push 7; Add; Trace;" in
   let result = interp program in
   print_endline ("Test Polynomial: " ^ (String.concat "; " (Option.get result)));
   assert (result = Some ["4"])
 
let test_de_morgans_law () =
   let program = "Push False; Push False; And; Not; Trace; Push False; Not; Push False; Not; Or; Trace;" in
   let result = interp program in
   print_endline ("Test De Morgan's Law: " ^ (String.concat "; " (Option.get result)));
   assert (result = Some ["True"; "True"])

let test_monotonic () =
   let program = "Push 2; Push 2; Mul; Push 3; Push 3; Mul; Gt; Trace;" in
   let result = interp program in
   print_endline ("Test Monotonicity of x^2: " ^ (String.concat "; " (Option.get result)));
   assert (result = Some ["True"])

let test_empty_stack_pop () =
   let program = "Pop;" in
   let result = interp program in
   print_endline ("Test Empty Stack Pop: " ^ (String.concat "; " (Option.get result)));
   assert (result = Some ["Panic"])

let test_invalid_addition () =
   let program = "Push True; Push 3; Add;" in
   let result = interp program in
   print_endline ("Test Invalid Addition: " ^ (String.concat "; " (Option.get result)));
   assert (result = Some ["Panic"])

let test_invalid_and () =
   let program = "Push 3; Push False; And;" in
   let result = interp program in
   print_endline ("Test Invalid And: " ^ (String.concat "; " (Option.get result)));
   assert (result = Some ["Panic"])
    
let test_swap_single_element () =
   let program = "Push 1; Swap;" in
   let result = interp program in
   print_endline ("Test Swap Single Element: " ^ (String.concat "; " (Option.get result)));
   assert (result = Some ["Panic"])
    
let () =
   print_endline "Starting tests...";
   test_polynomial ();
   test_de_morgans_law ();
   test_monotonic ();
   test_empty_stack_pop ();
   test_invalid_addition ();
   test_invalid_and ();
   test_swap_single_element ();
   print_endline "All tests passed successfully!"

(* ------------------------------------------------------------ *)