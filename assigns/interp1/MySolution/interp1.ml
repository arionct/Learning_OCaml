#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-1.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

(* Types for constants and commands *)

type constants =
  | Int of int
  | Bool of bool
  | Unit of unit

type commands = 
  | Push of constants | Pop | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt

let rec p_int acc = function
   | c :: cs when c >= '0' && c <= '9' -> p_int (10 * acc + (int_of_char c - int_of_char '0')) cs
   | cs -> acc

let exam_stack(cons : constants list * string list) : constants list =
   match cons with
   | (stack, _) -> stack

let exam(cons : constants list * string list) : string list =
   match cons with
   | (_, list_of_strings) -> list_of_strings

let rec dig_to_str(d : int) : string =
  let new_digit = char_of_digit d in 
  str(new_digit)

let rec int_to_str(i : int) : string =
  if (i = 0) then "0" else 
  let rec helper (num : int) (acc : string) =
  if num != 0 then
    let digit_selector = num mod 10 in
    let digit_string = dig_to_str digit_selector in
    helper (num/10) (string_append digit_string acc)
  else 
    acc
  in helper i ""

let toString(x : constants) : string =
  match x with
  | Int x -> int_to_str(x)
  | Bool x -> if (x = true) then "True" else "False"
  | Unit x -> "Unit"

let rec comm = function 
  | curr :: next when curr = ';' -> true
  | curr :: next when curr = ' ' || curr = '\n' || curr = '\r' || curr = '\t'  -> comm next
  | _ -> false

let rec str_to_prog(char_list : char list)(prog : commands list) : commands list option =
  match char_list with
  | ' ' :: rest | '\n' :: rest | '\r' :: rest | '\t' :: rest   | ';' :: rest -> str_to_prog rest prog
  | 'P' :: 'u' :: 's' :: 'h' :: rest -> (
    let rec helper rest = (
      match rest with
      | '-' :: x :: rest when x >= '1' && x <= '9' ->
        let n = (p_int 0 (x :: rest)) * -1 in
        str_to_prog rest (Push (Int n) :: prog)
      | x :: rest when x >= '0' && x <= '9' ->
        let n = p_int 0 (x :: rest) in
        str_to_prog rest (Push (Int n) :: prog)
      | 'T' :: 'r' :: 'u' :: 'e' :: rest -> str_to_prog rest (Push (Bool true) :: prog)
      | 'F' :: 'a' :: 'l' :: 's' :: 'e' :: rest -> str_to_prog rest (Push (Bool false) :: prog)
      | 'U' :: 'n' :: 'i' :: 't' :: rest -> str_to_prog rest (Push (Unit ()) :: prog)
      | ' ' :: rest | '\n' :: rest | '\r' :: rest |  '\t' :: rest -> helper rest
      | _ -> None
    ) in helper rest)
  | 'P' :: 'o' :: 'p' :: rest -> if comm (rest) then 
      str_to_prog rest (Pop :: prog) 
    else None
  | 'T' :: 'r' :: 'a' :: 'c' :: 'e' :: rest -> if comm (rest) then  
      str_to_prog rest (Trace :: prog) 
    else None
  | 'A' :: 'd' :: 'd' :: rest -> if comm (rest) then 
      str_to_prog rest (Add :: prog) 
    else None
  | 'S' :: 'u' :: 'b' :: rest -> if comm (rest) then 
      str_to_prog rest (Sub :: prog) 
    else None
  | 'M' :: 'u' :: 'l' :: rest -> if comm (rest) then 
      str_to_prog rest (Mul :: prog) 
    else None
  | 'D' :: 'i' :: 'v' :: rest -> if comm (rest) then 
      str_to_prog rest (Div :: prog) 
    else None
  | 'A' :: 'n' :: 'd' :: rest -> if comm (rest) then 
      str_to_prog rest (And :: prog) 
    else None
  | 'O' :: 'r' :: rest -> if comm (rest) then 
      str_to_prog rest (Or :: prog) 
    else None
  | 'N' :: 'o' :: 't' :: rest -> if comm (rest) then 
      str_to_prog rest (Not :: prog) 
    else None
  | 'L' :: 't' :: rest -> if comm (rest) then 
      str_to_prog rest (Lt :: prog) 
    else None
  | 'G' :: 't' :: rest -> if comm (rest) then 
      str_to_prog rest (Gt :: prog) 
    else None
  | x :: rest when x >= '0' && x <= '9' -> str_to_prog rest prog
  | [] -> Some prog
  | _ -> None

let push(cons : constants)(stack : constants list) : constants list =
  cons :: stack

let pop(stack : constants list)(output : string list) : constants list * string list =
  match stack with
  | cons :: rest -> (rest, output)
  | [] -> ([], "Panic" :: output)

let trace(stack : constants list)(output : string list): constants list * string list = 
  match stack with
  | x :: xs -> 
    let string_element = toString(x) in
    let stack = exam_stack(pop(stack)(output)) in
    let stack = push(Unit ())(stack) in
    let output = string_element :: output in
    (stack, output)
  | [] -> ([], "Panic" :: output)

let num_ops(stack : constants list)(operation: commands list)(output : string list) : constants list * string list =   
  match stack with
  | Int i :: Int j :: rest ->
    let result = 
      match operation with
      | Add :: _ -> Int (i + j)
      | Sub :: _ -> Int (i - j)
      | Mul :: _ -> Int (i * j)
      | Div :: rest when j != 0 -> Int (j / i)
      | Div :: rest when j = 0 -> Int (-11111111)
      | Lt :: _ -> Bool (i < j)
      | Gt :: _ -> Bool (i > j)
      | _ -> failwith "Invalid operation"
    in
    if result = Int (-11111111) then 
      ([], "Panic" :: output) 
    else
      let stack = exam_stack(pop (stack)(output)) in
      let stack = exam_stack(pop (stack)(output)) in
      let stack = push(result)(stack) in
      (stack, output)

  | _ :: (Int j) :: rest -> ([], "Panic" :: output)
  | (Int i) :: _ :: rest -> ([], "Panic" :: output)
  | (Int i) :: rest -> ([], "Panic" :: output)
  | [] -> ([], "Panic" :: output)
  | _ -> ([], "Panic" :: output)

let bool_ops (stack : constants list)(operation: commands list)(output : string list) : constants list * string list = 
  match stack with
  | Bool i :: Bool j :: rest ->
    let com =
      match operation with
      | And :: _ -> Bool (i && j)
      | Or :: _ -> Bool (i || j)
      | _ -> failwith "Invalid boolean operation"
    in
    let stack = exam_stack(pop (stack)(output)) in
    let stack = exam_stack(pop (stack)(output)) in
    let stack = push(com)(stack) in
    (stack, output)

  | _ :: (Bool j) :: rest -> ([], "Panic" :: output)
  | (Bool i) :: _ :: rest -> ([], "Panic" :: output)
  | (Bool i) :: rest -> ([], "Panic" :: output)
  | [] -> ([], "Panic" :: output)
  | _ -> ([], "Panic" :: output)

let not_op(stack : constants list)(output : string list) : constants list * string list =
  match stack with
  | Bool x :: rest ->
    let comb = Bool (not x) in
    let stack = exam_stack(pop(stack)(output)) in
    let stack = push(comb)(stack) in
    (stack, output)
  | [] -> ([], "Panic" :: output)
  | _ -> ([], "Panic" :: output)

let pani pan = pan = "Panic"

let interp (str : string) : string list option =
  let empty : commands list = [] in
  let stack : constants list = [] in
  let output : string list = [] in
  let options = str_to_prog (string_listize(str)) (empty) in

  match options with
  | Some reversal_program -> 
    let program = list_reverse reversal_program in
    let rec helper (current_prog : commands list) (stack_output : constants list * string list) : string list option =
      if (exam_stack(stack_output) = [] && list_exists (exam(stack_output)) pani) then
        Some (exam(stack_output))
      else
        match current_prog with
        | command :: rest ->
        (begin
          match command with
          |Push constant -> helper(rest)(push (constant) (exam_stack(stack_output)),exam(stack_output))
          |Pop -> helper(rest)(pop (exam_stack(stack_output)) (exam(stack_output)))
          |Trace -> helper(rest)(trace (exam_stack(stack_output)) (exam(stack_output)))
          |Add | Sub | Mul | Div | Lt | Gt -> helper(rest)(num_ops (exam_stack(stack_output)) (current_prog) (exam(stack_output)))
          |And | Or -> helper(rest)(bool_ops (exam_stack(stack_output)) (current_prog) (exam(stack_output)))
          |Not -> helper(rest)(not_op (exam_stack(stack_output)) (exam(stack_output)))
        end)
        | [] -> Some (exam(stack_output)) in helper program (stack, output)
  | None -> None