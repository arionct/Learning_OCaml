#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-2.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

(* abstract syntax tree of interp2 *)

type const =
  | Int of int
  | Bool of bool
  | Unit
  | Char of char
  | Sym of string
  | Closure of string * env * coms

and env = (string * const) list

and com =
  | Push of const | Pop | Swap | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt
  | If of coms * coms
  | Bind | Lookup
  | Fun of string * coms | Call | Return

and coms = com list

(* ------------------------------------------------------------ *)

(* parsers for interp2 *)

let parse_nat = 
  let* n = natural << whitespaces in pure n

let parse_int =
  (let* n = parse_nat in pure (Int n)) <|>
  (keyword "-" >> let* n = parse_nat in pure (Int (-n)))

let parse_bool =
  (keyword "True" >> pure (Bool true)) <|>
  (keyword "False" >> pure (Bool false))

let parse_unit =
  keyword "Unit" >> pure Unit

let parse_char =
   let is_lowercase_letter c = c >= 'a' && c <= 'z' in
   let* c = satisfy is_lowercase_letter << whitespaces in
   pure (Char c)

let is_sym_char c = 
   (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
   
let string_of_char_list char_list =
   let rec aux acc = function
      | [] -> acc
      | c :: cs -> aux (string_snoc acc c) cs
   in aux "" char_list;;
   
let check c =
   match c with
   | '\n' -> "\\n"
   | '\t' -> "\\t"
   | '\r' -> "\\r"
   | '\\' -> "\\\\" | '\'' -> "\\'"
   | _    -> str c

let letter_or_number c =
   (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

let ch =
   let* chars = many (satisfy letter_or_number) in
   pure (list_make_fwork (fun work -> list_foreach chars work))

let parse_str : string parser =
   fun ls ->
      match ch ls with
      | Some (charList, rest) ->
         Some (list_foldleft charList "" (fun acc c -> acc ^ check c), rest)
      | None -> None

let parse_sym = 
   let* n = parse_str << whitespaces in pure (Sym n)
    
let parse_const =
  parse_int <|>
  parse_bool <|>
  parse_unit <|>
  parse_char <|>
  parse_sym

let parse_if_else parse_coms =
   let* _ = keyword "If" in
   let* if_branch = parse_coms () in
   let* _ = keyword "Else" in
   let* else_branch = parse_coms () in
   let* _ = keyword "End" in
   pure (If (if_branch, else_branch))
 
let parse_sym_as_string =
   let* sym_chars = many1 (satisfy is_sym_char) << whitespaces in
   pure (string_of_char_list sym_chars)
   
let parse_fun parse_coms =
   let* _ = keyword "Fun" in
   let* sym = parse_sym_as_string in
   let* fun_body = parse_coms () in
   let* _ = keyword "End" in
   pure (Fun (sym, fun_body))
       
 
let parse_com parse_coms = 
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
   (parse_if_else parse_coms) <|>
   (keyword "Bind" >> pure Bind) <|>
   (keyword "Lookup" >> pure Lookup) <|>
   (parse_fun parse_coms) <|>
   (keyword "Call" >> pure Call) <|>
   (keyword "Return" >> pure Return)
 
let rec parse_coms () =
   many (parse_com parse_coms << keyword ";")
 

(* ------------------------------------------------------------ *)

(* interpreter *)

type stack = const list
type trace = string list
type env = (string * const) list
type prog = coms

let rec str_of_nat (n : int) : string =
  let d = n mod 10 in 
  let n0 = n / 10 in
  let s = str (chr (d + ord '0')) in 
  if 0 < n0 then
    string_append (str_of_nat n0) s
  else s

let str_of_int (n : int) : string = 
  if n < 0 then
    string_append "-" (str_of_nat (-n))
  else str_of_nat n


let str_of_char (c : char) : string =
   str(c)

let str_of_closure (name : string) (env : env) (cmds : coms) : string =
   let fun_str = "Fun" in
   let combined_str = string_cons '<' (string_append fun_str (string_snoc name '>')) in
   combined_str

let toString (c : const) : string =
  match c with
  | Int i -> str_of_int i
  | Bool true -> "True"
  | Bool false -> "False"
  | Unit -> "Unit"
  | Char ch -> str_of_char ch
  | Sym s -> s
  | Closure (name, env, cmds) -> str_of_closure name env cmds

let rec custom_lookup key env =
   match env with
   | [] -> None
   | (k, v) :: tail -> 
      if k = key then Some v 
      else custom_lookup key tail
 
let rec eval (s : stack) (t : trace) (v: env) (p : prog) : trace =
  match p with
  (* termination state returns the trace *)
  | [] -> t
  | Push c :: p0 (* PushStack *) -> eval (c :: s) t v p0
  | Pop :: p0 ->
    (match s with
     | _ :: s0 (* PopStack *) -> eval s0 t v p0
     | []      (* PopError *) -> eval [] ("Panic" :: t) v [])
  | Swap :: p0 ->
    (match s with
     | a :: b :: s0 (* SwapStack *) -> eval (b :: a :: s0) t v p0
     | _ :: []      (* SwapError2 *) -> eval [] ("Panic" :: t) v []
     | []           (* SwapError1 *) -> eval [] ("Panic" :: t) v [])
  | Trace :: p0 ->
    (match s with
     | c :: s0 (* TraceStack *) -> eval (Unit :: s0) (toString c :: t) v p0
     | []      (* TraceError *) -> eval [] ("Panic" :: t) v [])
  | Add :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* AddStack *)  -> eval (Int (i + j) :: s0) t v p0
     | _ :: _ :: s0         (* AddError1 *) -> eval [] ("Panic" :: t) v []
     | []                   (* AddError2 *) -> eval [] ("Panic" :: t) v []
     | _ :: []              (* AddError3 *) -> eval [] ("Panic" :: t) v [])
  | Sub :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* SubStack *)  -> eval (Int (i - j) :: s0) t v p0
     | _ :: _ :: s0         (* SubError1 *) -> eval [] ("Panic" :: t) v []
     | []                   (* SubError2 *) -> eval [] ("Panic" :: t) v []
     | _ :: []              (* SubError3 *) -> eval [] ("Panic" :: t) v [])
  | Mul :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* MulStack *)  -> eval (Int (i * j) :: s0) t v p0
     | _ :: _ :: s0         (* MulError1 *) -> eval [] ("Panic" :: t) v []
     | []                   (* MulError2 *) -> eval [] ("Panic" :: t) v []
     | _ :: []              (* MulError3 *) -> eval [] ("Panic" :: t) v [])
  | Div :: p0 ->
    (match s with
     | Int i :: Int 0 :: s0 (* DivError0 *) -> eval [] ("Panic" :: t) v []
     | Int i :: Int j :: s0 (* DivStack *)  -> eval (Int (i / j) :: s0) t v p0
     | _ :: _ :: s0         (* DivError1 *) -> eval [] ("Panic" :: t) v []
     | []                   (* DivError2 *) -> eval [] ("Panic" :: t) v []
     | _ :: []              (* DivError3 *) -> eval [] ("Panic" :: t) v [])
  | And :: p0 ->
    (match s with
     | Bool a :: Bool b :: s0 (* AndStack *)  -> eval (Bool (a && b) :: s0) t v p0
     | _ :: _ :: s0           (* AndError1 *) -> eval [] ("Panic" :: t) v []
     | []                     (* AndError2 *) -> eval [] ("Panic" :: t) v []
     | _ :: []                (* AndError3 *) -> eval [] ("Panic" :: t) v [])
  | Or :: p0 ->
    (match s with
     | Bool a :: Bool b :: s0 (* OrStack *)  -> eval (Bool (a || b) :: s0) t v p0
     | _ :: _ :: s0           (* OrError1 *) -> eval [] ("Panic" :: t) v []
     | []                     (* OrError2 *) -> eval [] ("Panic" :: t) v []
     | _ :: []                (* OrError3 *) -> eval [] ("Panic" :: t) v [])
  | Not :: p0 ->
    (match s with
     | Bool a :: s0 (* NotStack  *) -> eval (Bool (not a) :: s0) t v p0
     | _ :: s0      (* NotError1 *) -> eval [] ("Panic" :: t) v []
     | []           (* NotError2 *) -> eval [] ("Panic" :: t) v [])
  | Lt :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* LtStack *)  -> eval (Bool (i < j) :: s0) t v p0
     | _ :: _ :: s0         (* LtError1 *) -> eval [] ("Panic" :: t) v []
     | []                   (* LtError2 *) -> eval [] ("Panic" :: t) v []
     | _ :: []              (* LtError3 *) -> eval [] ("Panic" :: t) v [])
  | Gt :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* GtStack *)  -> eval (Bool (i > j) :: s0) t v p0
     | _ :: _ :: s0         (* GtError1 *) -> eval [] ("Panic" :: t) v []
     | []                   (* GtError2 *) -> eval [] ("Panic" :: t) v []
     | _ :: []              (* GtError3 *) -> eval [] ("Panic" :: t) v [])
  | If (if_branch, else_branch) :: p0 ->
    (match s with
     | Bool true :: s0  -> eval s0 t v (if_branch @ p0)
     | Bool false :: s0 -> eval s0 t v (else_branch @ p0)
     | _ :: s0          -> eval [] ("Panic" :: t) v []
     | []               -> eval [] ("Panic" :: t) v [])
  | Bind :: p0 ->
    (match s with
     | Sym sym_name :: const_value :: s0 -> 
         let new_env = (sym_name, const_value) :: v in
         eval s0 t new_env p0
     | _ :: _ :: s0 (* BindError1 *) -> eval [] ("Panic" :: t) v []
     | []           (* BindError2 *) -> eval [] ("Panic" :: t) v []
     | _ :: []      (* BindError3 *) -> eval [] ("Panic" :: t) v [])
  | Lookup :: p0 ->
    (match s with
     | Sym x :: s0 -> 
         (match custom_lookup x v with
         | Some value -> eval (value :: s0) t v p0
         | None       -> eval [] ("Panic" :: t) v []) (* LookupError3 *)
     | [] -> eval [] ("Panic" :: t) v [] (* LookupError2 *)
     | _  -> eval [] ("Panic" :: t) v []) (* LookupError1 *)
  | Fun (name, body) :: p0 ->
    (match s with
     | Sym x :: s0 -> eval ((Closure (x, v, body)) :: s0) t v p0
     | []          -> eval [] ("Panic" :: t) v [] (* FunError2 *)
     | _           -> eval [] ("Panic" :: t) v []) (* FunError1 *)
  | Call :: p0 ->
    (match s with
     | Closure (f, closureEnv, closureComs) :: a :: restOfStack -> 
         let newEnv = (f, Closure (f, closureEnv, closureComs)) :: closureEnv in
         let continuation = Closure ("cc", v, p0) in
         eval (a :: continuation :: restOfStack) t newEnv closureComs
     | []          -> eval [] ("Panic" :: t) v [] (* CallError2 *)
     | [_]         -> eval [] ("Panic" :: t) v [] (* CallError3 *)
     | _           -> eval [] ("Panic" :: t) v []) (* CallError1 *)
  | Return :: p0 ->
    (match s with
     | Closure (f, closureEnv, closureComs) :: a :: restOfStack ->
         eval (a :: restOfStack) t closureEnv closureComs
     | []                   -> eval [] ("Panic" :: t) v [] (* ReturnError2 *)
     | [_]                  -> eval [] ("Panic" :: t) v [] (* ReturnError3 *)
     | _                    -> eval [] ("Panic" :: t) v []) (* ReturnError1 *)
   
    

(* ------------------------------------------------------------ *)

(* putting it all together [input -> parser -> eval -> output] *)

let interp (s : string) : string list option =
  match string_parse (whitespaces >> parse_coms ()) s with
  | Some (p, []) -> Some (eval [] [] [] p)
  | _ -> None

(* ------------------------------------------------------------ *)

