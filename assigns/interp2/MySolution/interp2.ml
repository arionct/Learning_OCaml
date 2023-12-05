#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-2.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

(* abstract syntax tree of interp1 *)

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

(* parsers for interp1 *)

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
   String.concat "" (List.map (String.make 1) char_list)
    
let parse_sym =
   let* sym_chars = many1 (satisfy is_sym_char) << whitespaces in
   pure (Sym (string_of_char_list sym_chars))
    
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
   "Closure(" ^ name ^ ")"

let toString (c : const) : string =
  match c with
  | Int i -> str_of_int i
  | Bool true -> "True"
  | Bool false -> "False"
  | Unit -> "Unit"
  | Char ch -> str_of_char ch
  | Sym s -> s
  | Closure (name, env, cmds) -> str_of_closure name env cmds

let rec eval (s : stack) (t : trace) (v: env) (p : prog) : trace =
  match p with
  (* termination state returns the trace *)
  | [] -> t
  | Push c :: p0 (* PushStack *) -> eval (c :: s) t v p0
  | Pop :: p0 ->
    (match s with
     | _ :: s0 (* PopStack *) -> eval s0 t v p0
     | []      (* PopError *) -> eval [] ("Panic" :: t) [] [])
  | Swap :: p0 ->
    (match s with
     | a :: b :: s0 (* SwapStack *) -> eval (b :: a :: s0) t v p0
     | _ :: []      (* SwapError2 *) -> eval [] ("Panic" :: t) [] []
     | []           (* SwapError1 *) -> eval [] ("Panic" :: t) [] [])
  | Trace :: p0 ->
    (match s with
     | c :: s0 (* TraceStack *) -> eval (Unit :: s0) (toString c :: t) v p0
     | []      (* TraceError *) -> eval [] ("Panic" :: t) [] [])
  | Add :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* AddStack *)  -> eval (Int (i + j) :: s0) t v p0
     | _ :: _ :: s0         (* AddError1 *) -> eval [] ("Panic" :: t) [] []
     | []                   (* AddError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []              (* AddError3 *) -> eval [] ("Panic" :: t) [] [])
  | Sub :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* SubStack *)  -> eval (Int (i - j) :: s0) t v p0
     | _ :: _ :: s0         (* SubError1 *) -> eval [] ("Panic" :: t) [] []
     | []                   (* SubError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []              (* SubError3 *) -> eval [] ("Panic" :: t) [] [])
  | Mul :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* MulStack *)  -> eval (Int (i * j) :: s0) t v p0
     | _ :: _ :: s0         (* MulError1 *) -> eval [] ("Panic" :: t) [] []
     | []                   (* MulError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []              (* MulError3 *) -> eval [] ("Panic" :: t) [] [])
  | Div :: p0 ->
    (match s with
     | Int i :: Int 0 :: s0 (* DivError0 *) -> eval [] ("Panic" :: t) [] []
     | Int i :: Int j :: s0 (* DivStack *)  -> eval (Int (i / j) :: s0) t v p0
     | _ :: _ :: s0         (* DivError1 *) -> eval [] ("Panic" :: t) [] []
     | []                   (* DivError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []              (* DivError3 *) -> eval [] ("Panic" :: t) [] [])
  | And :: p0 ->
    (match s with
     | Bool a :: Bool b :: s0 (* AndStack *)  -> eval (Bool (a && b) :: s0) t v p0
     | _ :: _ :: s0           (* AndError1 *) -> eval [] ("Panic" :: t) [] []
     | []                     (* AndError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []                (* AndError3 *) -> eval [] ("Panic" :: t) [] [])
  | Or :: p0 ->
    (match s with
     | Bool a :: Bool b :: s0 (* OrStack *)  -> eval (Bool (a || b) :: s0) t v p0
     | _ :: _ :: s0           (* OrError1 *) -> eval [] ("Panic" :: t) [] []
     | []                     (* OrError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []                (* OrError3 *) -> eval [] ("Panic" :: t) [] [])
  | Not :: p0 ->
    (match s with
     | Bool a :: s0 (* NotStack  *) -> eval (Bool (not a) :: s0) t v p0
     | _ :: s0      (* NotError1 *) -> eval [] ("Panic" :: t) [] []
     | []           (* NotError2 *) -> eval [] ("Panic" :: t) [] [])
  | Lt :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* LtStack *)  -> eval (Bool (i < j) :: s0) t v p0
     | _ :: _ :: s0         (* LtError1 *) -> eval [] ("Panic" :: t) [] []
     | []                   (* LtError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []              (* LtError3 *) -> eval [] ("Panic" :: t) [] [])
  | Gt :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* GtStack *)  -> eval (Bool (i > j) :: s0) t v p0
     | _ :: _ :: s0         (* GtError1 *) -> eval [] ("Panic" :: t) [] []
     | []                   (* GtError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []              (* GtError3 *) -> eval [] ("Panic" :: t) [] [])
  | If (if_branch, else_branch) :: p0 ->
    (match s with
     | Bool true :: s0  -> eval s0 t v (if_branch @ p0)
     | Bool false :: s0 -> eval s0 t v (else_branch @ p0)
     | _ :: s0          -> eval [] ("Panic" :: t) [] []
     | []               -> eval [] ("Panic" :: t) [] [])
  | Bind :: p0 ->
    (match s with
     | Sym sym_name :: const_value :: s0 -> 
         let new_env = (sym_name, const_value) :: v in
         eval s0 t new_env p0
     | _ :: _ :: s0 (* BindError1 *) -> eval [] ("Panic" :: t) [] []
     | []           (* BindError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []      (* BindError3 *) -> eval [] ("Panic" :: t) [] [])
  | Lookup :: p0 ->
    (match s with
     | Sym x :: s0 -> 
         (match List.assoc_opt x v with
         | Some value -> eval (value :: s0) t v p0
         | None       -> eval [] ("Panic" :: t) [] []) (* LookupError3 *)
     | [] -> eval [] ("Panic" :: t) [] [] (* LookupError2 *)
     | _  -> eval [] ("Panic" :: t) [] []) (* LookupError1 *)
  | Fun (name, body) :: p0 ->
    (match s with
     | Sym x :: s0 -> eval ((Closure (x, v, body)) :: s0) t v p0
     | []          -> eval [] ("Panic" :: t) [] [] (* FunError2 *)
     | _           -> eval [] ("Panic" :: t) [] []) (* FunError1 *)
  | Call :: p0 ->
    (match s with
     | Closure (f, closureEnv, closureComs) :: a :: restOfStack -> 
         let newEnv = (f, Closure (f, closureEnv, closureComs)) :: closureEnv in
         let continuation = Closure ("cc", v, p0) in
         eval (a :: continuation :: restOfStack) t newEnv closureComs
     | []          -> eval [] ("Panic" :: t) [] [] (* CallError2 *)
     | [_]         -> eval [] ("Panic" :: t) [] [] (* CallError3 *)
     | _           -> eval [] ("Panic" :: t) [] []) (* CallError1 *)
  | Return :: p0 ->
    (match s with
     | Closure (f, closureEnv, closureComs) :: a :: restOfStack ->
         eval (a :: restOfStack) t closureEnv closureComs
     | []                   -> eval [] ("Panic" :: t) [] [] (* ReturnError2 *)
     | [_]                  -> eval [] ("Panic" :: t) [] [] (* ReturnError3 *)
     | _                    -> eval [] ("Panic" :: t) [] []) (* ReturnError1 *)
   
    

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
let test () =
   let test_program = 
   "Push -2; Trace;" (* Placeholder for test input string *)
   in match interp test_program with
      | Some trace -> List.iter print_endline trace
      | None -> print_endline "Program failed to interpret"
 
 let _ = test()