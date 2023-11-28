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
type digit = int
type nat = int
type bool = True | False
type const = Int of int | Bool of bool | Unit

type command =
  | Push of const
  | Pop
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

(* Helper function to parse a single character *)
let parse_char c s =
  if string_length s > 0 && s.[0] = c then Some (String.sub s 1 (string_length s - 1))
  else None

(* Helper function to parse a specific string *)
let rec parse_string str s =
  if string_length s >= string_length str && String.sub s 0 (string_length str) = str
  then Some (String.sub s (string_length str) (string_length s - string_length str))
  else None


(* Parser for a single digit *)
let parse_digit s =
   if string_length s > 0 && s.[0] >= '0' && s.[0] <= '9' then
     Some (int_of_char s.[0] - int_of_char '0', String.sub s 1 (string_length s - 1))
   else None
 
(* Parser for natural numbers *)
let rec parse_nat s =
   match parse_digit s with
   | Some (d, rest) ->
       (match parse_nat rest with
        | Some (n, rest') -> Some (d * 10 + n, rest')
        | None -> Some (d, rest))
   | None -> None
 
(* Parser for integers *)
let parse_int s =
   if string_length s > 0 && s.[0] = '-' then
     match parse_nat (String.sub s 1 (string_length s - 1)) with
     | Some (n, rest) -> Some (-n, rest)
     | None -> None
   else parse_nat s
 
(* Parser for boolean values *)
let parse_bool s =
   match parse_string "True" s with
   | Some rest -> Some (True, rest)
   | None ->
       (match parse_string "False" s with
        | Some rest -> Some (False, rest)
        | None -> None)
 
(* Parser for constants *)
let parse_const s =
   match parse_int s with
   | Some (n, rest) -> Some (Int n, rest)
   | None ->
       (match parse_bool s with
        | Some (b, rest) -> Some (Bool b, rest)
        | None ->
           (match parse_string "Unit" s with
            | Some rest -> Some (Unit, rest)
            | None -> None))

            
(* Parser for individual commands *)
let parse_command s =
   match parse_string "Push " s with
   | Some rest ->
       (match parse_const rest with
        | Some (c, rest') -> Some (Push c, rest')
        | None -> None)
   | None ->
       match parse_string "Pop" s with
       | Some rest -> Some (Pop, rest)
       | None -> 
           match parse_string "Trace" s with
           | Some rest -> Some (Trace, rest)
           | None ->
               match parse_string "Add" s with
               | Some rest -> Some (Add, rest)
               | None ->
                   match parse_string "Sub" s with
                   | Some rest -> Some (Sub, rest)
                   | None ->
                       match parse_string "Mul" s with
                       | Some rest -> Some (Mul, rest)
                       | None ->
                           match parse_string "Div" s with
                           | Some rest -> Some (Div, rest)
                           | None ->
                               match parse_string "And" s with
                               | Some rest -> Some (And, rest)
                               | None ->
                                   match parse_string "Or" s with
                                   | Some rest -> Some (Or, rest)
                                   | None ->
                                       match parse_string "Not" s with
                                       | Some rest -> Some (Not, rest)
                                       | None ->
                                           match parse_string "Lt" s with
                                           | Some rest -> Some (Lt, rest)
                                           | None ->
                                               match parse_string "Gt" s with
                                               | Some rest -> Some (Gt, rest)
                                               | None -> None

       
(* Parser for command sequences *)
let rec parse_commands s =
   if string_length s = 0 then Some ([], "")
   else
   match parse_command s with
   | Some (cmd, rest) ->
       (* Check for a semicolon to continue parsing further commands *)
       (match parse_char ';' rest with
      | Some rest' ->
          (match parse_commands rest' with
         | Some (cmds, rest'') -> Some (cmd :: cmds, rest'')
         | None -> None)
      | None -> Some ([cmd], rest)) (* No further commands *)
   | None -> None
 
(* Parser for programs *)
let parse_program s =
   match parse_commands s with
   | Some (cmds, "") -> Some cmds  (* Successfully parsed commands and no remaining string *)
   | _ -> None  (* Partially parsed commands or a parsing failure *)

 
 


(* State of the interpreter *)
type state = {
  stack: const list;
  trace: string list;
}

(* Helper function to convert const to string *)
let string_of_const = function
  | Int n -> string_of_int n
  | Bool True -> "True"
  | Bool False -> "False"
  | Unit -> "Unit"

(* Helper function to perform binary operations on integers *)
let binary_op f op1 op2 =
  match (op1, op2) with
  | (Int n1, Int n2) -> Some (Int (f n1 n2))
  | _ -> None

(* Function to execute a single command *)
let exec_command state command =
  match command with
  | Push c -> { state with stack = c :: state.stack }
  | Pop ->
      (match state.stack with
       | _ :: rest -> { state with stack = rest }
       | [] -> raise (Failure "Pop on empty stack"))
  | Trace ->
      (match state.stack with
       | c :: rest -> { stack = Unit :: rest; trace = (string_of_const c) :: state.trace }
       | [] -> raise (Failure "Trace on empty stack"))
   | Add ->
      (match state.stack with
         | a :: b :: rest ->
            (match binary_op (+) a b with
            | Some result -> { state with stack = result :: rest }
            | None -> raise (Failure "Invalid operands for Add"))
      | _ -> raise (Failure "Insufficient operands for Add"))
   | Sub ->
      (match state.stack with
         | a :: b :: rest ->
            (match binary_op (-) a b with
            | Some result -> { state with stack = result :: rest }
            | None -> raise (Failure "Invalid operands for Sub"))
      | _ -> raise (Failure "Insufficient operands for Sub"))
   | Mul ->
      (match state.stack with
         | a :: b :: rest ->
            (match binary_op ( * ) a b with
            | Some result -> { state with stack = result :: rest }
            | None -> raise (Failure "Invalid operands for Mul"))
      | _ -> raise (Failure "Insufficient operands for Mul"))
   | Div ->
      (match state.stack with
         | a :: b :: rest ->
            (match binary_op (/) a b with
            | Some result -> { state with stack = result :: rest }
            | None -> raise (Failure "Invalid operands for Div"))
      | _ -> raise (Failure "Insufficient operands for Div"))
   | And ->
      (match state.stack with
         | a :: b :: rest ->
            (match (a, b) with
            | (Bool True, Bool True) -> { state with stack = Bool True :: rest }
            | (Bool False, Bool False) -> { state with stack = Bool False :: rest }
            | _ -> raise (Failure "Invalid operands for And"))
      | _ -> raise (Failure "Insufficient operands for And"))
   | Or ->
      (match state.stack with
         | a :: b :: rest ->
            (match (a, b) with
            | (Bool True, Bool True) -> { state with stack = Bool True :: rest }
            | (Bool False, Bool False) -> { state with stack = Bool False :: rest }
            | _ -> raise (Failure "Invalid operands for Or"))
      | _ -> raise (Failure "Insufficient operands for Or"))
   | Not ->
      (match state.stack with
         | a :: rest ->
            (match a with
            | Bool True -> { state with stack = Bool False :: rest }
            | Bool False -> { state with stack = Bool True :: rest }
            | _ -> raise (Failure "Invalid operand for Not"))
      | _ -> raise (Failure "Insufficient operands for Not"))
   | Lt ->
      (match state.stack with
         | a :: b :: rest ->
            (match (a, b) with
            | (Int n1, Int n2) -> { state with stack = (if n1 < n2 then Bool True else Bool False) :: rest }
            | _ -> raise (Failure "Invalid operands for Lt"))
      | _ -> raise (Failure "Insufficient operands for Lt"))
   | Gt ->
      (match state.stack with
         | a :: b :: rest ->
            (match (a, b) with
            | (Int n1, Int n2) -> { state with stack = (if n1 > n2 then Bool True else Bool False) :: rest }
            | _ -> raise (Failure "Invalid operands for Gt"))
      | _ -> raise (Failure "Insufficient operands for Gt"))
  | _ -> raise (Failure "Unimplemented command")

(* Function to execute a sequence of commands *)
let rec exec_commands state commands =
  match commands with
  | [] -> state
  | cmd :: rest ->
      let new_state = exec_command state cmd in
      exec_commands new_state rest

(* Interpreter function *)
let interp (s : string) : string list option =
  match parse_program s with
  | Some cmds ->
      let initial_state = { stack = []; trace = [] } in
      let final_state = exec_commands initial_state cmds in
      Some (List.rev final_state.trace)
  | None -> None
