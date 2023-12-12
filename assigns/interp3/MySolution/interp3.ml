#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the [compile] function following the
specifications described in CS320_Fall_2023_Project-3.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

(* ------------------------------------------------------------ *)

(* abstract syntax tree of high-level language *)

type uopr =
  | Neg | Not

type bopr =
  | Add | Sub | Mul | Div | Mod
  | And | Or
  | Lt  | Gt  | Lte | Gte | Eq

type expr =
  | Int of int
  | Bool of bool
  | Unit
  | UOpr of uopr * expr
  | BOpr of bopr * expr * expr
  | Var of string
  | Fun of string * string * expr
  | App of expr * expr
  | Let of string * expr * expr
  | Seq of expr * expr
  | Ifte of expr * expr * expr
  | Trace of expr

(* ------------------------------------------------------------ *)

(* combinator for left-associative operators *)

let chain_left (p : 'a parser) (q : ('a -> 'a -> 'a) parser) : 'a parser =
  let* init = p in
  let* fms = many (let* f = q in let* m = p in pure (f, m)) in
  let m = list_foldleft fms init (fun acc (f, m) -> f acc m) in
  pure m

let rec chain_right (p : 'a parser) (q : ('a -> 'a -> 'a) parser) : 'a parser =
  let* m = p in
  (let* f = q in
   let* rest = chain_right p q in
   pure (f m rest)) <|> 
  (pure m)

let opt (p : 'a parser) : 'a option parser =
  (let* x = p in pure (Some x)) <|> pure None

(* basic constants *)

let parse_int : expr parser =
  let* n = natural in
  pure (Int n) << whitespaces

let parse_bool : expr parser =
  (keyword "true" >> pure (Bool true)) <|>
  (keyword "false" >> pure (Bool false))

let parse_unit : expr parser =
  keyword "(" >> keyword ")" >> pure Unit

(* names *)

let isReserved s =
  let reserved = 
    ["let"; "rec"; "in"; "fun"; "if"; "then"; "else"; "trace"; "mod"; "not"] 
  in
  list_exists reserved (fun s0 -> s0 = s)

let parse_name : string parser =
  let lower = satisfy char_islower in
  let upper = satisfy char_isupper in
  let digit = satisfy char_isdigit in
  let quote = char '\'' in
  let wildc = char '_' in
  let* c = lower <|> wildc in
  let* cs = many (lower <|> upper <|> digit <|> wildc <|> quote) in
  let s = string_make_fwork (list_foreach (c :: cs)) in
  if isReserved s then fail
  else pure s << whitespaces

(* unary operators *)

let parse_neg : (expr -> expr) parser =
  keyword "-" >> pure (fun m -> UOpr (Neg, m))

(* binary operators *)

let parse_add : (expr -> expr -> expr) parser =
  keyword "+" >> pure (fun m n -> BOpr (Add, m, n))

let parse_sub : (expr -> expr -> expr) parser =
  keyword "-" >> pure (fun m n -> BOpr (Sub, m, n))

let parse_mul : (expr -> expr -> expr) parser =
  keyword "*" >> pure (fun m n -> BOpr (Mul, m, n))

let parse_div : (expr -> expr -> expr) parser =
  keyword "/" >> pure (fun m n -> BOpr (Div, m, n))

let parse_mod : (expr -> expr -> expr) parser =
  keyword "mod" >> pure (fun m n -> BOpr (Mod, m, n))

let parse_and : (expr -> expr -> expr) parser =
  keyword "&&" >> pure (fun m n -> BOpr (And, m, n))

let parse_or : (expr -> expr -> expr) parser =
  keyword "||" >> pure (fun m n -> BOpr (Or, m, n))

let parse_lt : (expr -> expr -> expr) parser =
  keyword "<" >> pure (fun m n -> BOpr (Lt, m, n))

let parse_gt : (expr -> expr -> expr) parser =
  keyword ">" >> pure (fun m n -> BOpr (Gt, m, n))

let parse_lte : (expr -> expr -> expr) parser =
  keyword "<=" >> pure (fun m n -> BOpr (Lte, m, n))

let parse_gte : (expr -> expr -> expr) parser =
  keyword ">=" >> pure (fun m n -> BOpr (Gte, m, n))

let parse_eq : (expr -> expr -> expr) parser =
  keyword "=" >> pure (fun m n -> BOpr (Eq, m, n))

let parse_neq : (expr -> expr -> expr) parser =
  keyword "<>" >> pure (fun m n -> UOpr (Not, BOpr (Eq, m, n)))

let parse_seq : (expr -> expr -> expr) parser =
  keyword ";" >> pure (fun m n -> Seq (m, n))

(* expression parsing *)

let rec parse_expr () = 
  let* _ = pure () in
  parse_expr9 ()

and parse_expr1 () : expr parser = 
  let* _ = pure () in
  parse_int <|> 
  parse_bool <|> 
  parse_unit <|>
  parse_var () <|>
  parse_fun () <|>
  parse_letrec () <|>
  parse_let () <|>
  parse_ifte () <|>
  parse_trace () <|>
  parse_not () <|>
  (keyword "(" >> parse_expr () << keyword ")")

and parse_expr2 () : expr parser =
  let* m = parse_expr1 () in
  let* ms = many' parse_expr1 in
  let m = list_foldleft ms m (fun acc m -> App (acc, m)) in
  pure m

and parse_expr3 () : expr parser =
  let* f_opt = opt parse_neg in
  let* m = parse_expr2 () in
  match f_opt with
  | Some f -> pure (f m)
  | None -> pure m

and parse_expr4 () : expr parser =
  let opr = parse_mul <|> parse_div <|> parse_mod in
  chain_left (parse_expr3 ()) opr

and parse_expr5 () : expr parser =
  let opr = parse_add <|> parse_sub in
  chain_left (parse_expr4 ()) opr

and parse_expr6 () : expr parser =
  let opr = 
    parse_lte <|> 
    parse_gte <|>
    parse_neq <|>
    parse_lt <|> 
    parse_gt <|>
    parse_eq
  in
  chain_left (parse_expr5 ()) opr

and parse_expr7 () : expr parser =
  chain_left (parse_expr6 ()) parse_and

and parse_expr8 () : expr parser =
  chain_left (parse_expr7 ()) parse_or

and parse_expr9 () : expr parser =
  chain_right (parse_expr8 ()) parse_seq

and parse_var () : expr parser =
  let* x = parse_name in
  pure (Var x)

and parse_fun () : expr parser =
  let* _ = keyword "fun" in
  let* xs = many1 parse_name in 
  let* _ = keyword "->" in
  let* body = parse_expr () in
  let m = list_foldright xs body (fun x acc -> Fun ("", x, acc)) in
  pure m

and parse_let () : expr parser =
  let* _ = keyword "let" in
  let* x = parse_name in
  let* xs = many parse_name in
  let* _ = keyword "=" in
  let* body = parse_expr () in
  let* _ = keyword "in" in
  let* n = parse_expr () in
  let m = list_foldright xs body (fun x acc -> Fun ("", x, acc)) in
  pure (Let (x, m, n))

and parse_letrec () : expr parser =
  let* _ = keyword "let" in
  let* _ = keyword "rec" in
  let* f = parse_name in
  let* x = parse_name in
  let* xs = many parse_name in
  let* _ = keyword "=" in
  let* body = parse_expr () in
  let* _ = keyword "in" in
  let* n = parse_expr () in
  let m = list_foldright xs body (fun x acc -> Fun ("", x, acc)) in
  pure (Let (f, Fun (f, x, m), n))

and parse_ifte () : expr parser =
  let* _ = keyword "if" in
  let* m = parse_expr () in
  let* _ = keyword "then" in
  let* n1 = parse_expr () in
  let* _ = keyword "else" in
  let* n2 = parse_expr () in
  pure (Ifte (m, n1, n2))

and parse_trace () : expr parser =
  let* _ = keyword "trace" in
  let* m = parse_expr1 () in
  pure (Trace m) 

and parse_not () : expr parser =
  let* _ = keyword "not" in
  let* m = parse_expr1 () in
  pure (UOpr (Not, m))

exception SyntaxError
exception UnboundVariable of string

type scope = (string * string) list

let new_var =
  let stamp = ref 0 in
  fun x ->
    incr stamp;
    let xvar = string_filter x (fun c -> c <> '_' && c <> '\'') in
    string_concat_list ["v"; xvar; "i"; string_of_int !stamp]

let find_var scope s =
  let rec loop scope =
    match scope with
    | [] -> None
    | (s0, x) :: scope ->
      if s = s0 then Some x
      else loop scope
  in loop scope

let scope_expr (m : expr) : expr = 
  let rec aux scope m =
    match m with
    | Int i -> Int i
    | Bool b -> Bool b
    | Unit -> Unit
    | UOpr (opr, m) -> UOpr (opr, aux scope m)
    | BOpr (opr, m, n) -> 
      let m = aux scope m in
      let n = aux scope n in
      BOpr (opr, m, n)
    | Var s -> 
      (match find_var scope s with
       | None -> raise (UnboundVariable s)
       | Some x -> Var x)
    | Fun (f, x, m) -> 
      let fvar = new_var f in
      let xvar = new_var x in
      let m = aux ((f, fvar) :: (x, xvar) :: scope) m in
      Fun (fvar, xvar, m)
    | App (m, n) ->
      let m = aux scope m in
      let n = aux scope n in
      App (m, n)
    | Let (x, m, n) ->
      let xvar = new_var x in
      let m = aux scope m in
      let n = aux ((x, xvar) :: scope) n in
      Let (xvar, m, n)
    | Seq (m, n) ->
      let m = aux scope m in
      let n = aux scope n in
      Seq (m, n)
    | Ifte (m, n1, n2) ->
      let m = aux scope m in
      let n1 = aux scope n1 in
      let n2 = aux scope n2 in
      Ifte (m, n1, n2)
    | Trace m -> Trace (aux scope m)
  in
  aux [] m

(* ------------------------------------------------------------ *)

(* parser for the high-level language *)

let parse_prog (s : string) : expr =
  match string_parse (whitespaces >> parse_expr ()) s with
  | Some (m, []) -> scope_expr m
  | _ -> raise SyntaxError


(* ------------------------------------------------------------ *)

(* compiler *)

let (^) = string_append

let rec compile_expr scope = function
  | Int n -> "Push " ^ string_of_int n ^ ";\n"
  | Bool b -> "Push " ^ (if b then "True" else "False") ^ ";\n"
  | Unit -> "Push Unit;\n"
  | Var x -> (
    match find_var scope x with
    | None -> raise (UnboundVariable x)
    | Some v -> "Push " ^ v ^ ";\nLookup;\n"
  )
  | Seq (expr1, expr2) ->
    (compile_expr scope expr1) ^ "\nPop;\n" ^ (compile_expr scope expr2)
  | Ifte (cond, expr_then, expr_else) ->
    comp_if scope cond expr_then expr_else
  | Fun (f, x, m) ->
    comp_fun scope f x m
  | App (m, n) ->
    (compile_expr scope m) ^ "\n" ^ (compile_expr scope n) ^ "\nSwap;\nCall;"
  | Let (x, m, n) ->
    comp_let scope x m n
  | Trace expr ->
    (compile_expr scope expr) ^ "\nTrace;"
  | UOpr (Neg, expr) -> (compile_expr scope expr) ^ "\nPush -1;\nMul;"
  | UOpr (Not, expr) -> (compile_expr scope expr) ^ "\nNot;"
  | BOpr (Add, expr1, expr2) -> (compile_expr scope expr1) ^ "\n" ^ (compile_expr scope expr2) ^ "\nSwap;\nAdd;"
  | BOpr (Sub, expr1, expr2) -> (compile_expr scope expr1) ^ "\n" ^ (compile_expr scope expr2) ^ "\nSwap;\nSub;"
  | BOpr (Mul, expr1, expr2) -> (compile_expr scope expr1) ^ "\n" ^ (compile_expr scope expr2) ^ "\nSwap;\nMul;"
  | BOpr (Div, expr1, expr2) -> (compile_expr scope expr1) ^ "\n" ^ (compile_expr scope expr2) ^ "\nSwap;\nDiv;"
  | BOpr (And, expr1, expr2) -> (compile_expr scope expr1) ^ "\n" ^ (compile_expr scope expr2) ^ "\nAnd;"
  | BOpr (Or, expr1, expr2) -> (compile_expr scope expr1) ^ "\n" ^ (compile_expr scope expr2) ^ "\nOr;"
  | BOpr (Mod, expr1, expr2) -> comp_mod scope expr1 expr2
  | BOpr (Lt, expr1, expr2) -> (compile_expr scope expr1) ^ "\n" ^ (compile_expr scope expr2) ^ "\nSwap;\nLt;"
  | BOpr (Gt, expr1, expr2) -> (compile_expr scope expr1) ^ "\n" ^ (compile_expr scope expr2) ^ "\nSwap;\nGt;"
  | BOpr (Lte, expr1, expr2) -> (compile_expr scope (BOpr (Gt, expr1, expr2))) ^ "\nNot;"
  | BOpr (Gte, expr1, expr2) -> (compile_expr scope (BOpr (Lt, expr1, expr2))) ^ "\nNot;"
  | BOpr (Eq, expr1, expr2) -> comp_eq scope expr1 expr2
and comp_if scope cond expr_then expr_else =
  let ccond = compile_expr scope cond in
  let cthen = compile_expr scope expr_then in
  let celse = compile_expr scope expr_else in
  ccond ^ "\n" ^ "If " ^ cthen ^ " Else " ^ celse ^ " End;\n"
and comp_fun scope f x m =
  let fvar = new_var f in
  let fscope = (f, fvar) :: scope in
  let xvar = new_var x in
  let xscope = (x, xvar) :: fscope in
  let a = compile_expr xscope m in
  "Push " ^ fvar ^ "; Fun Push " ^ xvar ^ "; Bind; " ^ a ^ " Swap; Return; End;\n"
and comp_let scope x m n =
  let xvar = new_var x in
  let a = compile_expr scope m in
  let b = compile_expr ((x, xvar) :: scope) n in
  a ^ "Push " ^ xvar ^ "; Bind; " ^ b
and comp_mod scope a b =
  let c1 = compile_expr scope a in
  let c2 = compile_expr scope b in
  let d = compile_expr scope (BOpr (Div, a, b)) in
  d ^ c2 ^ "Mul; " ^ c1 ^ "Sub;\n"
and comp_eq scope a b =
  let c1 = compile_expr scope (BOpr (Lt, a, b)) in
  let c2 = compile_expr scope (BOpr (Gt, a, b)) in
  c1 ^ "Not; " ^ c2 ^ "Not; And;\n"

let compile (s: string) : string =
  let ast = parse_prog s in
  compile_expr [] (scope_expr (ast))


(* Test Function *)
let test_compile input expected_trace =
  let output = compile input in
  print_string ("Test: " ^ input ^ "\n");
  print_string ("Expected: " ^ expected_trace ^ "\n");
  print_string ("Got: " ^ output ^ "\n\n")

(* Test Cases *)
let () =
  test_compile "trace 1; trace 2" "[\"2\", \"1\"]";
  test_compile "let rec fact x = if x <= 0 then 1 else x * fact (x - 1) in trace (fact 10)" "[\"3628800\"]";
  test_compile "let fibo x = let rec loop i a b = trace a; if i < x then loop (i + 1) b (a + b) else a in loop 0 0 1 in trace (fibo 10)" "[\"55\", \"55\", \"34\", \"21\", \"13\", \"8\", \"5\", \"3\", \"2\", \"1\", \"1\", \"0\"]";
  test_compile "let eff x = trace x in let foo x y z = () in foo (eff 1) (eff 2) (eff 3)" "[\"3\", \"2\", \"1\"]";
  test_compile "let rec mccarthy n = if n > 100 then n - 10 else mccarthy (mccarthy (n + 11)) in trace (mccarthy 22)" "[\"91\"]";
